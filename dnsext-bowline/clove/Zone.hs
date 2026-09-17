{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Zone (
    newZones,
    updateZone,
    findZoneAlist,
    toZoneAlist,
    zoneDirectory,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Function (on)
import Data.IORef
import Data.IP
import Data.IP.RouteTable
import Data.List
import Data.Maybe
import GHC.Event
import System.Directory (createDirectoryIfMissing)
import qualified System.IO.Error as E
import Text.Read

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Log
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types

import Algo
import qualified Axfr
import Config
import Exception
import KeyFile
import Serial
import Types

----------------------------------------------------------------

newZones :: [ZoneConf] -> IO [Zone]
newZones zcs = do
    checkDuplicate $ map (fromRepresentation . cnf_zone) zcs
    mapM newZone zcs

-- | Refusing to serve the same zone twice.  Two entries with the same
--   name share a directory, so they overwrite each other's serial and
--   key files, and they are told apart inconsistently: a query goes to
--   the most specific match while a transfer or a notify goes to the
--   first entry listed, so the access control of one entry ends up
--   guarding the data of the other.
checkDuplicate :: [Domain] -> IO ()
checkDuplicate zones = case nub (zones \\ nub zones) of
    [] -> return ()
    ds -> E.ioError $ E.userError $ "duplicate zone: " ++ unwords (map toRepresentation ds)

----------------------------------------------------------------

newZone :: ZoneConf -> IO Zone
newZone zoneconf@ZoneConf{..} = do
    -- Whether the zone is signed is decided by the configuration alone.
    -- It must not depend on whether the initial load happens to succeed,
    -- otherwise a transient failure would silently turn the zone into an
    -- unsigned one for the whole life time of the process.  A bad signing
    -- configuration is fatal instead of being degraded into "unsigned".
    msigning <- withZoneName $ readSigning zone zoneconf
    -- The source is not read here.  Reading it can block for as long as
    -- an unreachable upstream takes to time out, and nothing is
    -- listening yet at this point, so every other zone would be
    -- unreachable for that whole time too.  syncZone loads it.
    -- Until then the zone is not ready, which is answered with SERVFAIL.
    -- Each switch gates its own address list.  Listing addresses is not
    -- by itself a permission: "allow-transfer: no" must deny the
    -- transfer even when allow-transfer-addrs is not empty.
    let (a4, a6)
            | cnf_allow_transfer = readIPRange cnf_allow_transfer_addrs
            | otherwise = ([], [])
        t4 = fromList $ map (,True) a4
        t6 = fromList $ map (,True) a6
        notify_addrs
            | cnf_notify = readIP cnf_notify_addrs
            | otherwise = []
        allow_notify_addrs
            | cnf_allow_notify = readIP cnf_allow_notify_addrs
            | otherwise = []
    (wakeup, wait) <- initSync
    return $
        Zone
            { zoneDB = emptyDB
            , zoneRRs = []
            , zoneReady = False
            , zoneFromFile = fromFile source
            , zoneNotifyAddrs = notify_addrs
            , zoneNotifyPort = cnf_notify_port
            , zoneAllowNotifyAddrs = allow_notify_addrs
            , zoneAllowTransfer4 = t4
            , zoneAllowTransfer6 = t6
            , zoneName = zone
            , zoneSource = source
            , zoneSigning = msigning
            , zoneWakeUp = wakeup
            , zoneTimeoutWait = wait
            }
  where
    zone = fromRepresentation cnf_zone
    source = readSource zoneconf
    withZoneName action =
        action `E.catchIOError` \e ->
            E.ioError $ E.ioeSetErrorString e (cnf_zone ++ ": " ++ E.ioeGetErrorString e)

fromFile :: Source -> Bool
fromFile (FromFile _) = True
fromFile _ = False

initSync :: IO (WakeUp, TimeoutWait)
initSync = do
    var <- newTVarIO False
    tmgr <- getSystemTimerManager
    return (wakeup var, wait var tmgr)
  where
    wakeup var = atomically $ writeTVar var True
    wait var tmgr mtout = case mtout of
        Nothing -> waitBody var
        Just tout -> E.bracket (register tout) cancel $ \_ -> waitBody var
      where
        register tout = registerTimeout tmgr (tout * 1000000) $ wakeup var
        cancel = unregisterTimeout tmgr
    waitBody var = atomically $ do
        v <- readTVar var
        check v
        writeTVar var False

----------------------------------------------------------------

updateZone :: Env -> IORef Zone -> IO ()
updateZone env zoneref = handleLogErr env WARNING () $ do
    Zone{..} <- readIORef zoneref
    (db, rrs) <- loadSourceWithSigning env zoneName zoneSource zoneSigning zoneRRs
    atomicModifyIORef' zoneref $ modify db rrs
  where
    modify db rrs zone = (zone', ())
      where
        zone' =
            zone
                { zoneReady = True
                , zoneDB = db
                , zoneRRs = rrs
                }

----------------------------------------------------------------

-- | Directory holding the per-zone state, that is the serial file and
--   the key files.  It must exist before anything is stored into it.
--
--   The trailing dot of the zone is dropped, so that \"example.jp.\"
--   becomes \"example.jp\".  That would leave the root zone with an
--   empty path, so it gets a name of its own.  Two zones must never be
--   given the same directory -- they would overwrite each other's
--   serial and keys -- and the trailing dot of that name is what makes
--   it safe: dropping the trailing dot of a representation can never
--   leave another one, because a zone has no empty label.  Plain
--   \"root\" would have collided with the zone \"root.\".
zoneDirectory :: Domain -> FilePath
zoneDirectory zone = case toRepresentation zone of
    "." -> "root."
    rep -> init rep -- dropping the trailing dot

-- | Rebuilding the zone database, signing it again if it is a signed
--   one.  The records passed in are the ones obtained last time; they
--   are used again when the source turns out to have nothing new, so
--   that signing again never waits on the source changing.
--   This function throws 'AuthException'.
loadSourceWithSigning
    :: Env
    -> Domain
    -> Source
    -> Maybe Signing
    -> [ResourceRecord]
    -> IO (DB, [ResourceRecord])
loadSourceWithSigning env zone source Nothing oldRRs = do
    let zoneDir = zoneDirectory zone
    createDirectoryIfMissing True zoneDir
    mserial <- loadSerial zoneDir
    rrs <- reloadSource env zone mserial source oldRRs
    db <- makeDBforSecondary zone rrs
    saveSerial zoneDir $ soa_serial $ fst $ dbSOA db
    return (db, rrs)
loadSourceWithSigning env zone source (Just Signing{..}) oldRRs = do
    let zoneDir = zoneDirectory zone
    createDirectoryIfMissing True zoneDir
    mserial <- loadSerial zoneDir
    rrs0 <- reloadSource env zone mserial source oldRRs
    (soa0, soarr0, rrs) <- checkRRs rrs0
    let ttl = soa_minimum soa0
        soa
            | byMySelf source = case mserial of
                Nothing -> soa0 -- No serial file, serial from zone file
                Just s -> soa0{soa_serial = s <> Serial 1}
            | otherwise = soa0
        soarr = soarr0{rdata = toRData soa}
    let kskKeyConfig = signingKSKConfig{keyConfTTL = ttl}
    (keyInfoKSK, dnskeyrr) <- loadKSKInfo zoneDir kskKeyConfig
    signKey <- makeSigner kskKeyConfig keyInfoKSK
    let zskKeyConfig = signingZSKConfig{keyConfTTL = ttl}
    ((_keyInfoZSK0, dnskeyrr0), (keyInfoZSK1, dnskeyrr1), (_keyInfoZSK2, dnskeyrr2)) <-
        loadZSKInfo zoneDir signingZSKPreserve zskKeyConfig
    signZone <- makeSigner zskKeyConfig keyInfoZSK1
    db <- makeDBforPrimary zone signingN3P signKey signZone (soarr : rrs ++ [dnskeyrr, dnskeyrr0, dnskeyrr1, dnskeyrr2])
    -- Stored only after the zone has been built successfully so that a
    -- failure does not inflate the serial.
    saveSerial zoneDir $ soa_serial soa
    return (db, rrs0)

byMySelf :: Source -> Bool
byMySelf (FromFile _) = True
byMySelf _ = False

-- | Reading the source, falling back on the records obtained last time
--   when the source has nothing new.
reloadSource
    :: Env
    -> Domain
    -> Maybe Serial
    -> Source
    -> [ResourceRecord]
    -> IO [ResourceRecord]
reloadSource env zone mserial source oldRRs =
    fromMaybe oldRRs <$> loadSource env zone sinceSerial source
  where
    -- With nothing to fall back on there is nothing to be gained by
    -- asking only for what is newer: fetch the zone whatever the
    -- stored serial says.
    sinceSerial
        | null oldRRs = Nothing
        | otherwise = mserial

-- | 'Nothing' means the source has nothing newer than the serial given.
--   This function throws 'AuthException'.
loadSource :: Env -> Domain -> Maybe Serial -> Source -> IO (Maybe [ResourceRecord])
loadSource env zone mserial source = case source of
    FromUpstream4 ip4 port -> Axfr.client env mserial (IPv4 ip4) port zone
    FromUpstream6 ip6 port -> Axfr.client env mserial (IPv6 ip6) port zone
    FromFile fn -> Just <$> loadZoneFile zone fn

checkRRs :: [ResourceRecord] -> IO (RD_SOA, ResourceRecord, [ResourceRecord])
checkRRs [] = E.ioError $ E.userError "No RRs"
checkRRs (soarr : rrs) = case fromRData $ rdata soarr of
    Nothing -> E.ioError $ E.userError "SOA does not exist"
    Just soa -> return (soa, soarr, rrs)

----------------------------------------------------------------

readIP :: [String] -> [IP]
readIP ss = mapMaybe readMaybe ss

readIPRange :: [String] -> ([AddrRange IPv4], [AddrRange IPv6])
readIPRange ss0 = loop id id ss0
  where
    loop b4 b6 [] = (b4 [], b6 [])
    loop b4 b6 (s : ss)
        | Just a6 <- readMaybe s = loop b4 (b6 . (a6 :)) ss
        | Just a4 <- readMaybe s = loop (b4 . (a4 :)) b6 ss
        | otherwise = loop b4 b6 ss

readSource :: ZoneConf -> Source
readSource ZoneConf{..}
    | Just a6 <- readMaybe cnf_source = FromUpstream6 a6 cnf_source_port
    | Just a4 <- readMaybe cnf_source = FromUpstream4 a4 cnf_source_port
    | otherwise = FromFile cnf_source

readSigning :: Domain -> ZoneConf -> IO (Maybe Signing)
readSigning dom ZoneConf{..}
    | not cnf_signing = return Nothing
    | otherwise = do
        checkDurations cnf_rrsig_lifetime cnf_zsk_rollover_duration
        checkPreserve cnf_zsk_preserve
        kskAlgo <- case toPubAlgo cnf_ksk_algo of
            Just pa0 -> return pa0
            Nothing -> E.ioError $ E.userError $ "Public Algo: " ++ cnf_ksk_algo ++ " is unknown"
        zskAlgo <- case toPubAlgo cnf_zsk_algo of
            Just pa0 -> return pa0
            Nothing -> E.ioError $ E.userError $ "Public Algo: " ++ cnf_zsk_algo ++ " is unknown"
        checkAlgorithms kskAlgo zskAlgo
        dd <- case toDsDigest cnf_ds_digest of
            Just dd0 -> return dd0
            Nothing -> E.ioError $ E.userError $ "DS Digest: " ++ cnf_ds_digest ++ " is unknown"
        let keyConfKSK =
                KeyConfig
                    { keyConfZone = dom
                    , keyConfPubAlg = kskAlgo
                    , keyConfDigestAlg = dd
                    , keyConfTTL = 3600 -- overridden by SOA
                    , keyConfLifetime = toDNSTime $ fromIntegral cnf_rrsig_lifetime
                    , keyConfType = KSK
                    , keyConfSize = cnf_ksk_size
                    }
        let keyConfZSK =
                KeyConfig
                    { keyConfZone = dom
                    , keyConfPubAlg = zskAlgo
                    , keyConfDigestAlg = dd
                    , keyConfTTL = 3600 -- overridden by SOA
                    , keyConfLifetime = toDNSTime $ fromIntegral cnf_rrsig_lifetime
                    , keyConfType = ZSK
                    , keyConfSize = cnf_zsk_size
                    }
        h <- case toNsec3Hash cnf_nsec3_hash of
            Just h0 -> return h0
            Nothing -> E.ioError $ E.userError $ "NSEC3 Hash: " ++ cnf_nsec3_hash ++ " is unknown"
        let mn3p
                | cnf_nsec3 = Just $ defaultNSEC3PARAM{nsec3param_hashalg = h}
                | otherwise = Nothing
        return $
            Just $
                Signing
                    { signingKSKConfig = keyConfKSK
                    , signingZSKConfig = keyConfZSK
                    , signingZSKRollover = cnf_zsk_rollover_duration
                    , signingZSKPreserve = cnf_zsk_preserve
                    , signingN3P = mn3p
                    }

----------------------------------------------------------------

-- | Finding the zone a name belongs to.  A name below a delegation
--   point belongs to the zone below it, so the most specific of the
--   configured zones wins.  Taking the first match instead made
--   \"www.sub.example.jp\" land in \"example.jp\" whenever that zone
--   happened to be written first in the configuration file.
-- | Shortest a duration may be, in seconds.  A key is published a whole
--   rollover duration before it starts signing, so that duration has to
--   cover the TTL of the DNSKEY RRset for resolvers to have the key by
--   then, and those TTLs are hours; and a signature which lives less
--   than this is of no use with any sensible TTL either.
minDuration :: Int
minDuration = 3600

-- | The zone is re-signed once per rollover duration, so a signature
--   has to outlive that interval with room to spare -- otherwise it
--   expires in the gap before the next one is made.  A third of the
--   interval is the room asked for here.
checkDurations :: Int -> Int -> IO ()
checkDurations lifetime rollover
    | rollover < minDuration =
        failWith $
            "zsk-rollover-duration must be at least "
                ++ show minDuration
                ++ " seconds, but is "
                ++ show rollover
    | lifetime < minDuration =
        failWith $
            "rrsig-lifetime must be at least "
                ++ show minDuration
                ++ " seconds, but is "
                ++ show lifetime
    | 3 * lifetime < 4 * rollover =
        failWith $
            "rrsig-lifetime must be at least "
                ++ show needed
                ++ " seconds, a third longer than zsk-rollover-duration ("
                ++ show rollover
                ++ "), but is "
                ++ show lifetime
    | otherwise = return ()
  where
    needed = (4 * rollover + 2) `div` 3
    failWith = E.ioError . E.userError

-- | Refusing a KSK and a ZSK of different algorithms.
--
--   RFC 4035 Sec 2.2 asks for an RRSIG on every RRset by at least one
--   key of every algorithm in the apex DNSKEY RRset.  clove signs the
--   DNSKEY RRset with the KSK and everything else with the ZSK, one
--   algorithm each, so a zone whose two keys differ would advertise two
--   algorithms while signing nothing with both.  A validator which
--   implements only one of them could then follow no path through the
--   zone at all.
--
--   Signing with two algorithms at once is what an algorithm rollover
--   needs, and clove cannot express it: one KSK and one line of ZSKs is
--   all it keeps.  Better to say so than to produce a zone that looks
--   signed and is not usable.
checkAlgorithms :: PubAlg -> PubAlg -> IO ()
checkAlgorithms ksk zsk
    | ksk == zsk = return ()
    | otherwise =
        E.ioError $
            E.userError $
                "ksk-algo and zsk-algo must name the same algorithm, but are "
                    ++ show ksk
                    ++ " and "
                    ++ show zsk

-- | Fewest ZSKs which may be kept.  Three of them are published at any
--   time -- the previous key, the one signing and the next one -- so
--   keeping fewer would have every load generate a whole new set.
minPreserve :: Int
minPreserve = 3

checkPreserve :: Int -> IO ()
checkPreserve preserve
    | preserve < minPreserve =
        E.ioError $
            E.userError $
                "zsk-preserve must be at least "
                    ++ show minPreserve
                    ++ ", the number of ZSKs published at a time, but is "
                    ++ show preserve
    | otherwise = return ()

----------------------------------------------------------------

findZoneAlist :: Domain -> ZoneAlist -> Maybe (Domain, IORef Zone)
findZoneAlist dom alist = case filter (\(k, _) -> dom `isSubDomainOf` k) alist of
    [] -> Nothing
    xs -> Just $ maximumBy (compare `on` (labelsCount . fst)) xs

toZoneAlist :: [Zone] -> IO ZoneAlist
toZoneAlist zones = do
    refs <- mapM newIORef zones
    return $ zip names refs
  where
    names = map zoneName zones

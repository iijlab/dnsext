{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Zone (
    newZones,
    updateZone,
    findZoneAlist,
    toZoneAlist,
    zoneDirectory,
    zoneLabel,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import Data.Function (on)
import Data.IORef
import Data.IP
import Data.IP.RouteTable
import Data.List
import Data.Maybe
import GHC.Event
import System.Directory (createDirectoryIfMissing)
import qualified System.IO.Error as E
import System.Posix.Time (epochTime)
import Text.Read

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Log
import DNS.SEC
import DNS.SEC.Verify
import DNS.TSIG (TSIGKey)
import DNS.Types
import DNS.Types.Time (EpochTime)

import Algo
import qualified Axfr
import Config
import Exception
import KeyFile
import Serial
import TSIGKeys
import Types

----------------------------------------------------------------

newZones :: Env -> TSIGKeys -> [ZoneConf] -> IO [Zone]
newZones env keys zcs = do
    checkDuplicate $ map (fromRepresentation . cnf_zone) zcs
    mapM (newZone env keys) zcs

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

newZone :: Env -> TSIGKeys -> ZoneConf -> IO Zone
newZone env keys zoneconf@ZoneConf{..} = do
    -- Whether the zone is signed is decided by the configuration alone.
    -- It must not depend on whether the initial load happens to succeed,
    -- otherwise a transient failure would silently turn the zone into an
    -- unsigned one for the whole life time of the process.  A bad signing
    -- configuration is fatal instead of being degraded into "unsigned".
    msigning <- withZoneName $ readSigning env zone zoneconf
    notifyKey <- withZoneName $ namedKey keys "notify-key" cnf_notify_key
    allowNotifyKey <- withZoneName $ namedKey keys "allow-notify-key" cnf_allow_notify_key
    sourceKey <- withZoneName $ namedKey keys "source-key" cnf_source_key
    transferKey <- withZoneName $ namedKey keys "allow-transfer-key" cnf_allow_transfer_key
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
    -- Nothing has been read yet, so the expire is counted from now: a
    -- source which never answers must not leave the zone waiting for a
    -- refresh which already happened.
    now <- currentTime
    return $
        Zone
            { zoneDB = emptyDB
            , zoneRRs = []
            , zoneReady = False
            , zoneFromFile = fromFile source
            , zoneAnswered = now
            , zoneFailing = False
            , zoneNotifyAddrs = notify_addrs
            , zoneNotifyPort = cnf_notify_port
            , zoneAllowNotifyAddrs = allow_notify_addrs
            , zoneNotifyKey = notifyKey
            , zoneAllowNotifyKey = allowNotifyKey
            , zoneSourceKey = sourceKey
            , zoneTransferKey = transferKey
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
            E.ioError $ E.ioeSetErrorString e (zoneLabel zone ++ E.ioeGetErrorString e)

-- | Finding the key a setting names.  Naming one which is not in the
--   key file is a mistake worth stopping for: the alternative is a zone
--   which quietly goes on without the TSIG somebody asked for.
namedKey :: TSIGKeys -> String -> String -> IO (Maybe TSIGKey)
namedKey keys setting name
    | null name = return Nothing
    | otherwise = case lookupTSIGKey (fromRepresentation name) keys of
        Just k -> return $ Just k
        Nothing ->
            E.ioError $
                E.userError $
                    setting ++ ": no key named " ++ name ++ " in the key file"

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
updateZone env zoneref = do
    zone <- readIORef zoneref
    now <- currentTime
    er <- trySync $ loadSourceWithSigning env zone
    case er of
        -- Nothing was read, so the zone stays as it was -- for as long
        -- as it may.
        Left se -> do
            logSomeErrIn env WARNING (zoneLabel $ zoneName zone) se
            ready <- stillOurs now zone
            store $ \z -> z{zoneReady = ready, zoneFailing = True}
        Right Loaded{..}
            | loadedAnswered ->
                store $ \z ->
                    z
                        { zoneReady = True
                        , zoneDB = loadedDB
                        , zoneRRs = loadedRRs
                        , zoneAnswered = now
                        , zoneFailing = False
                        }
            -- The source said nothing, but what was read last time may
            -- have been signed again, so the database is taken all the
            -- same.
            | otherwise -> do
                ready <- stillOurs now zone
                store $ \z -> z{zoneReady = ready, zoneDB = loadedDB, zoneRRs = loadedRRs, zoneFailing = True}
  where
    store f = atomicModifyIORef' zoneref $ \z -> (f z, ())
    -- RFC 1035 Sec 3.3.13: the expire is the longest a secondary may
    -- go on answering for a zone whose source has stopped answering.
    -- Past it the zone is not ours to speak for, and it is answered the
    -- way a zone which never loaded is -- SERVFAIL, without the
    -- authoritative bit.  A zone read from a file has no source to lose
    -- and never expires.
    stillOurs now zone
        | zoneFromFile zone = pure $ zoneReady zone
        | not (zoneReady zone) = pure False
        | age <= expire = pure True
        | otherwise = do
            envPutLines
                env
                WARNING
                Nothing
                [ zoneLabel (zoneName zone)
                    ++ "the source has not answered for "
                    ++ show age
                    ++ " seconds, past the expire of "
                    ++ show expire
                    ++ ": the zone is not ours to answer for any more"
                ]
            pure False
      where
        age = now - zoneAnswered zone
        expire = fromIntegral $ soa_expire $ dbRD_SOA $ zoneDB zone

currentTime :: IO EpochTime
currentTime = fromIntegral . fromEnum <$> epochTime

----------------------------------------------------------------

-- | What a failure was about, for the log to carry: a server holding
--   several zones says little by reporting that some file or other
--   could not be read.
zoneLabel :: Domain -> String
zoneLabel zone = toRepresentation zone ++ ": "

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

-- | What reading the source came to.
data Loaded = Loaded
    { loadedDB :: DB
    , loadedRRs :: [ResourceRecord]
    , loadedAnswered :: Bool
    -- ^ Whether the source answered.  False when it could not be reached
    --   or would not be believed, in which case what is here is what was
    --   here before, signed again where the zone is signed.
    }

-- | Rebuilding the zone database, signing it again if it is a signed
--   one.  The records passed in are the ones obtained last time; they
--   are used again when the source turns out to have nothing new, so
--   that signing again never waits on the source changing.
--   This function throws 'AuthException'.
loadSourceWithSigning :: Env -> Zone -> IO Loaded
loadSourceWithSigning env z = case zoneSigning z of
    Nothing -> unsigned
    Just signing -> signed signing
  where
    zone = zoneName z
    source = zoneSource z
    oldRRs = zoneRRs z
    key = zoneSourceKey z
    zoneDir = zoneDirectory zone

    unsigned = do
        createDirectoryIfMissing True zoneDir
        mserial <- loadSerial zoneDir
        (answered, rrs) <- reloadSource env key zone mserial source oldRRs
        db <- makeDBforSecondary zone rrs
        saveSerial zoneDir $ soa_serial $ fst $ dbSOA db
        return $ Loaded db rrs answered

    signed Signing{..} = do
        createDirectoryIfMissing True zoneDir
        mserial <- loadSerial zoneDir
        (answered, rrs0) <- reloadSource env key zone mserial source oldRRs
        (soa0, soarr0, rrs) <- checkRRs rrs0
        checkUnsigned rrs
        let soa
                | byMySelf source = case mserial of
                    Nothing -> soa0 -- No serial file, serial from zone file
                    Just sr -> soa0{soa_serial = sr <> Serial 1}
                | otherwise = soa0
            soarr = soarr0{rdata = toRData soa}
            -- TTL of the DNSKEY RRset: the zone's own, taken from the
            -- apex SOA.  Not the SOA minimum, which RFC 2308 Sec 4
            -- redefined as the negative caching TTL and which is
            -- commonly a few minutes; no rule makes it the TTL of the
            -- keys.  NSEC3 does take it, and makeDBforPrimary uses it
            -- there (RFC 5155 Sec 3).
            keyTTL = rrttl soarr0
            kskKeyConfig = signingKSKConfig{keyConfTTL = keyTTL}
            zskKeyConfig = signingZSKConfig{keyConfTTL = keyTTL}
        (keyInfoKSK, dnskeyrr) <- loadKSKInfo zoneDir kskKeyConfig
        signKey <- makeSigner kskKeyConfig keyInfoKSK
        ((_keyInfoZSK0, dnskeyrr0), (keyInfoZSK1, dnskeyrr1), (_keyInfoZSK2, dnskeyrr2)) <-
            loadZSKInfo zoneDir signingZSKPreserve zskKeyConfig
        signZone <- makeSigner zskKeyConfig keyInfoZSK1
        db <-
            makeDBforPrimary zone signingN3P signKey signZone $
                soarr : rrs ++ [dnskeyrr, dnskeyrr0, dnskeyrr1, dnskeyrr2]
        -- Stored only after the zone has been built successfully so
        -- that a failure does not inflate the serial.
        saveSerial zoneDir $ soa_serial soa
        return $ Loaded db rrs0 answered

-- | Refusing to sign a zone which is signed already.
--
--   The source of a signed zone is expected to be the bare zone.  When
--   it carries RRSIGs, signing it again signs those RRSIGs too, and
--   publishes the upstream's keys beside ours, its NSEC3 chain beside
--   ours, and its SOA beside ours -- a zone which answers plausibly and
--   is thoroughly wrong.  Measured on a signed secondary of a signed
--   primary: eight DNSKEYs, two NSEC3PARAMs and two SOAs.
--
--   The zone is left as it was, which for a zone already serving means
--   it goes on serving what it had.
checkUnsigned :: [ResourceRecord] -> IO ()
checkUnsigned rrs
    | any ((== RRSIG) . rrtype) rrs =
        E.ioError $
            E.userError $
                "the source is signed already, so it is not signed again."
                    ++ "  Set signing to no to serve it as it comes, or take the"
                    ++ " signatures out of the source."
    | otherwise = return ()

byMySelf :: Source -> Bool
byMySelf (FromFile _) = True
byMySelf _ = False

-- | Reading the source, falling back on the records obtained last time
--   when the source has nothing new -- or nothing to say at all.  The
--   flag says which of the two it was: a source which did not answer
--   leaves the zone one refresh nearer its expire.
reloadSource
    :: Env
    -> Maybe TSIGKey
    -> Domain
    -> Maybe Serial
    -> Source
    -> [ResourceRecord]
    -> IO (Bool, [ResourceRecord])
reloadSource env key zone mserial source oldRRs =
    said <$> loadSource env key zone sinceSerial source
  where
    said (Transferred rrs) = (True, rrs)
    said Unchanged = (True, oldRRs)
    said Unreachable = (False, oldRRs)
    -- With nothing to fall back on there is nothing to be gained by
    -- asking only for what is newer: fetch the zone whatever the
    -- stored serial says.
    sinceSerial
        | null oldRRs = Nothing
        | otherwise = mserial

-- | Going to the source for the zone.  A file is always there to be
--   read; an upstream may have nothing newer, or nothing to say.
--   This function throws 'AuthException'.
loadSource
    :: Env
    -> Maybe TSIGKey
    -> Domain
    -> Maybe Serial
    -> Source
    -> IO FromUpstream
loadSource env key zone mserial source = case source of
    FromUpstream4 ip4 port -> Axfr.client env key mserial (IPv4 ip4) port zone
    FromUpstream6 ip6 port -> Axfr.client env key mserial (IPv6 ip6) port zone
    FromFile fn -> Transferred <$> loadZoneFile zone fn

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

readSigning :: Env -> Domain -> ZoneConf -> IO (Maybe Signing)
readSigning env dom ZoneConf{..}
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
        checkKeySize env "ksk" kskAlgo cnf_ksk_size
        checkKeySize env "zsk" zskAlgo cnf_zsk_size
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

-- | Whether an algorithm takes a key size at all.  The others have one
--   fixed by the curve they are named after.
isRSA :: PubAlg -> Bool
isRSA alg = alg `elem` [RSASHA1, RSASHA1_NSEC3_SHA1, RSASHA256, RSASHA512]

-- | Narrowest and widest RSA modulus, in bits.  RFC 3110 Sec 2 puts it
--   between 512 and 4096 for DNS.  The floor here is higher: 512 bits
--   has not been safe for a very long time, and RFC 8624 Sec 3.1 asks
--   for 2048.
minKeySize, maxKeySize :: Int
minKeySize = 1024
maxKeySize = 4096

-- | Checking the size of an RSA key.
--
--   Leaving it at the default of nothing gave a key crypton refused to
--   generate at all, and a few hundred bits gave one too small to hold
--   a SHA-256 signature -- in both cases after start up, as a bare
--   CryptoError_PrimeSizeInvalid or SignatureTooLong naming neither the
--   zone nor the setting, with the zone left on SERVFAIL.
--
--   The size is in bits and the generator divides it by eight, so a
--   size which is not a multiple of eight quietly yields a smaller key
--   than was asked for.
checkKeySize :: Env -> String -> PubAlg -> Int -> IO ()
checkKeySize env what alg size
    -- Not an error: a size left over from another algorithm does no
    -- harm.  But the operator plainly expects it to be used, so it is
    -- worth saying that it is not.
    | not (isRSA alg) =
        when (size /= 0) $
            envPutLines
                env
                WARNING
                Nothing
                [what ++ "-size is not used by " ++ show alg ++ ", but is set to " ++ show size]
    | size < minKeySize || size > maxKeySize =
        failWith $
            what
                ++ "-size must be between "
                ++ show minKeySize
                ++ " and "
                ++ show maxKeySize
                ++ " bits for "
                ++ show alg
                ++ ", but is "
                ++ show size
    | size `mod` 8 /= 0 =
        failWith $
            what ++ "-size must be a multiple of 8, but is " ++ show size
    | otherwise = return ()
  where
    failWith = E.ioError . E.userError

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

-- | Finding the zone a name belongs to.  A name below a delegation
--   point belongs to the zone below it, so the most specific of the
--   configured zones wins.  Taking the first match instead made
--   \"www.sub.example.jp\" land in \"example.jp\" whenever that zone
--   happened to be written first in the configuration file.
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

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

newZones :: Env -> [ZoneConf] -> IO [Zone]
newZones env zcs = mapM (newZone env) zcs

----------------------------------------------------------------

newZone :: Env -> ZoneConf -> IO Zone
newZone env zoneconf@ZoneConf{..} = do
    (db, ready, msigning) <- handleLogErr env WARNING (emptyDB, False, Nothing) $ do
        msigning' <- readSigning zone zoneconf
        db' <- loadSourceWithSigning env zone source msigning'
        return (db', True, msigning')
    let (a4, a6) = readIPRange cnf_allow_transfer_addrs
        t4 = fromList $ map (,True) a4
        t6 = fromList $ map (,True) a6
        notify_addrs = readIP cnf_notify_addrs
        allow_notify_addrs = readIP cnf_allow_notify_addrs
    (wakeup, wait) <- initSync
    return $
        Zone
            { zoneDB = db
            , zoneReady = ready
            , zoneFromFile = fromFile source
            , zoneNotifyAddrs = notify_addrs
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
    db <- loadSourceWithSigning env zoneName zoneSource zoneSigning
    atomicModifyIORef' zoneref $ modify db
  where
    modify db zone = (zone', ())
      where
        zone' =
            zone
                { zoneReady = True
                , zoneDB = db
                }

----------------------------------------------------------------

-- | Directory holding the per-zone state, that is the serial file and
--   the key files.  It must exist before anything is stored into it.
zoneDirectory :: Domain -> FilePath
zoneDirectory zone = case toRepresentation zone of
    "." -> "root"
    rep -> init rep -- dropping the trailing dot

-- | This function throws 'AuthException'.
loadSourceWithSigning
    :: Env
    -> Domain
    -> Source
    -> Maybe Signing
    -> IO DB
loadSourceWithSigning env zone source Nothing = do
    let zoneDir = zoneDirectory zone
    createDirectoryIfMissing True zoneDir
    mserial <- loadSerial zoneDir
    db <- loadSource env zone mserial source >>= makeDBforSecondary zone
    saveSerial zoneDir $ soa_serial $ fst $ dbSOA db
    return db
loadSourceWithSigning env zone source (Just Signing{..}) = do
    let zoneDir = zoneDirectory zone
    createDirectoryIfMissing True zoneDir
    mserial <- loadSerial zoneDir
    (soa0, soarr0, rrs) <- loadSource env zone mserial source >>= checkRRs
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
    ((_keyInfoZSK0, dnskeyrr0), (keyInfoZSK1, dnskeyrr1), (_keyInfoZSK2, dnskeyrr2)) <- loadZSKInfo zoneDir zskKeyConfig
    signZone <- makeSigner zskKeyConfig keyInfoZSK1
    db <- makeDBforPrimary zone signingN3P signKey signZone (soarr : rrs ++ [dnskeyrr, dnskeyrr0, dnskeyrr1, dnskeyrr2])
    -- Stored only after the zone has been built successfully so that a
    -- failure does not inflate the serial.
    saveSerial zoneDir $ soa_serial soa
    return db

byMySelf :: Source -> Bool
byMySelf (FromFile _) = True
byMySelf _ = False

-- | This function throws 'AuthException'.
loadSource :: Env -> Domain -> Maybe Serial -> Source -> IO [ResourceRecord]
loadSource env zone mserial source = case source of
    FromUpstream4 ip4 -> Axfr.client env mserial (IPv4 ip4) zone
    FromUpstream6 ip6 -> Axfr.client env mserial (IPv6 ip6) zone
    FromFile fn -> loadZoneFile zone fn

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
    | Just a6 <- readMaybe cnf_source = FromUpstream6 a6
    | Just a4 <- readMaybe cnf_source = FromUpstream4 a4
    | otherwise = FromFile cnf_source

readSigning :: Domain -> ZoneConf -> IO (Maybe Signing)
readSigning dom ZoneConf{..}
    | not cnf_signing = return Nothing
    | otherwise = do
        kskAlgo <- case toPubAlgo cnf_ksk_algo of
            Just pa0 -> return pa0
            Nothing -> E.ioError $ E.userError $ "Public Algo: " ++ cnf_ksk_algo ++ " is unknown"
        zskAlgo <- case toPubAlgo cnf_zsk_algo of
            Just pa0 -> return pa0
            Nothing -> E.ioError $ E.userError $ "Public Algo: " ++ cnf_zsk_algo ++ " is unknown"
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
                    , signingN3P = mn3p
                    }

----------------------------------------------------------------

findZoneAlist :: Domain -> ZoneAlist -> Maybe (Domain, IORef Zone)
findZoneAlist dom alist = find (\(k, _) -> dom `isSubDomainOf` k) alist

toZoneAlist :: [Zone] -> IO ZoneAlist
toZoneAlist zones = do
    refs <- mapM newIORef zones
    return $ zip names refs
  where
    names = map zoneName zones

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Zone (
    newZones,
    updateZone,
    findZoneAlist,
    toZoneAlist,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Data.IP
import Data.IP.RouteTable
import Data.List
import Data.Maybe
import Data.UnixTime
import GHC.Event
import System.Directory
import System.FilePath
import Text.Read

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Log
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import Algo
import qualified Axfr
import Config
import Types

----------------------------------------------------------------

newZones :: Env -> [ZoneConf] -> IO [Zone]
newZones env zcs = mapM (newZone env) zcs

----------------------------------------------------------------

newZone :: Env -> ZoneConf -> IO Zone
newZone env zoneconf@ZoneConf{..} = do
    msigning <- readSigning zone zoneconf
    edb <- E.try $ loadSourceWithSigning env zone (Serial 0) source msigning
    (db, ready) <- case edb of
        Left (AuthException msg) -> do
            envPutLines env WARNING Nothing [msg]
            return (emptyDB, False)
        Right db' -> return (db', True)
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
            , zoneShouldRefresh = shouldReload source
            , zoneNotifyAddrs = notify_addrs
            , zoneAllowNotifyAddrs = allow_notify_addrs
            , zoneAllowTransfer4 = t4
            , zoneAllowTransfer6 = t6
            , zoneName = zone
            , zoneSource = source
            , zoneSigning = msigning
            , zoneWakeUp = wakeup
            , zoneWait = wait
            }
  where
    zone = fromRepresentation cnf_zone
    source = readSource zoneconf

shouldReload :: Source -> Bool
shouldReload (FromFile _) = False
shouldReload _ = True

initSync :: IO (WakeUp, Wait)
initSync = do
    var <- newTVarIO False
    tmgr <- getSystemTimerManager
    return (wakeup var, wait var tmgr)
  where
    wakeup var = atomically $ writeTVar var True
    wait var tmgr tout
        | tout == 0 = waitBody var
        | otherwise = E.bracket register cancel $ \_ -> waitBody var
      where
        register = registerTimeout tmgr (tout * 1000000) $ wakeup var
        cancel = unregisterTimeout tmgr
    waitBody var = atomically $ do
        v <- readTVar var
        check v
        writeTVar var False

----------------------------------------------------------------

updateZone :: Env -> IORef Zone -> IO ()
updateZone env zoneref = do
    Zone{..} <- readIORef zoneref
    let serial = soa_serial $ dbRD_SOA zoneDB
    edb <- E.try $ loadSourceWithSigning env zoneName serial zoneSource zoneSigning
    case edb of
        Left (AuthException msg) -> envPutLines env WARNING Nothing [msg]
        Right db -> atomicModifyIORef' zoneref $ modify db
  where
    modify db zone = (zone', ())
      where
        zone' =
            zone
                { zoneReady = True
                , zoneDB = db
                }

----------------------------------------------------------------

extractTTL :: [ResourceRecord] -> IO Seconds
extractTTL [] = E.throwIO $ AuthException "No RRs"
extractTTL (soarr : _rest) = case fromRData $ rdata soarr of
    Nothing -> E.throwIO $ AuthException "SOA does not exist"
    Just soa -> return $ soa_minimum soa

-- | This function throws 'AuthException'.
loadSourceWithSigning
    :: Env
    -> Domain
    -> Serial
    -> Source
    -> Maybe Signing
    -> IO DB
loadSourceWithSigning env zone serial source Nothing =
    loadSource env zone serial source >>= makeDBforSecondary zone
loadSourceWithSigning env zone serial source (Just (Signing info mn3p)) = do
    rrs <- loadSource env zone serial source
    ttl <- extractTTL rrs
    let info' = info{dnssecInfoTTL = ttl}
    (pub, pri, dnskeyrr, dsrr, doSign) <- prepareDNSSEC info'
    saveKSKFile zone pub pri dsrr
    makeDBforPrimary zone mn3p doSign (rrs ++ [dnskeyrr])

-- | This function throws 'AuthException'.
loadSource :: Env -> Domain -> Serial -> Source -> IO [ResourceRecord]
loadSource env zone serial source = case source of
    FromUpstream4 ip4 -> Axfr.client env serial (IPv4 ip4) zone
    FromUpstream6 ip6 -> Axfr.client env serial (IPv6 ip6) zone
    FromFile fn -> loadZoneFile zone fn

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
        pa <- case toPubAlgo cnf_pub_algo of
            Just pa0 -> return pa0
            Nothing -> E.throwIO $ AuthException $ "Public Algo: " ++ cnf_pub_algo ++ " is unknown"
        dd <- case toDsDigest cnf_ds_digest of
            Just dd0 -> return dd0
            Nothing -> E.throwIO $ AuthException $ "DS Digest: " ++ cnf_ds_digest ++ " is unknown"
        let info =
                DNSSECinfo
                    { dnssecInfoZone = dom
                    , dnssecInfoPubAlg = pa
                    , dnssecInfoDigestAlg = dd
                    , dnssecInfoTTL = 3600 -- overridden by SOA
                    , dnssecInfoDuration = 86400 -- fixme
                    }
        h <- case toNsec3Hash cnf_nsec3_hash of
            Just h0 -> return h0
            Nothing -> E.throwIO $ AuthException $ "NSEC3 Hash: " ++ cnf_nsec3_hash ++ " is unknown"
        let mn3p
                | cnf_nsec3 = Just $ defaultNSEC3PARAM{nsec3param_hashalg = h}
                | otherwise = Nothing
        return $ Just $ Signing info mn3p

----------------------------------------------------------------

findZoneAlist :: Domain -> ZoneAlist -> Maybe (Domain, IORef Zone)
findZoneAlist dom alist = find (\(k, _) -> dom `isSubDomainOf` k) alist

toZoneAlist :: [Zone] -> IO ZoneAlist
toZoneAlist zones = do
    refs <- mapM newIORef zones
    return $ zip names refs
  where
    names = map zoneName zones

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
saveKSKFile :: Domain -> PubKey -> C8.ByteString -> ResourceRecord -> IO ()
saveKSKFile zone pub pri dsrr = do
    let zoneDir = init $ toRepresentation zone
    createDirectoryIfMissing True zoneDir
    case fromRData $ rdata dsrr of
        Nothing -> return ()
        Just RD_DS{..} -> do
            let statusBS =
                    "Zone:       " <> toRepresentation zone <> "\n" <>
                    "KeyTag:     " <> toB ds_key_tag <> "\n" <>
                    "Algorithm:  " <> toB (fromPubAlg ds_pubalg)  <> " # " <> toB ds_pubalg <> "\n" <>
                    "DigestAlgo: " <> toB (fromDigestAlg ds_digestalg) <> " # " <> toB ds_digestalg <> "\n" <>
                    "Digest:     " <> Opaque.toBase16 ds_digest <> "\n" <>
                    "PublicKey:  " <> Opaque.toBase16 (fromPubKey pub) <> "\n" <>
                    "PrivateKey: " <> B16.encode pri <> "\n"
            t <- getUnixTime
            fn <- C8.unpack <$> formatUnixTime "%Y-%m-%d-%H:%M:%S.ksk" t
            C8.writeFile (zoneDir </> fn) statusBS
  where
    toB :: Show a => a -> C8.ByteString
    toB = C8.pack . show
{- FOURMOLU_ENABLE -}

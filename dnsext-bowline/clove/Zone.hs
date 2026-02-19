{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Zone where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.IORef
import Data.IP
import Data.IP.RouteTable
import Data.List
import Data.Maybe
import GHC.Event
import Text.Read

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Types

import qualified Axfr
import Types

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

readSource :: String -> Source
readSource s
    | Just a6 <- readMaybe s = FromUpstream6 a6
    | Just a4 <- readMaybe s = FromUpstream4 a4
    | otherwise = FromFile s

----------------------------------------------------------------

loadSource :: Domain -> Serial -> Source -> IO (Maybe DB)
loadSource zone serial source = case source of
    FromUpstream4 ip4 -> toDB <$> Axfr.client serial (IPv4 ip4) zone
    FromUpstream6 ip6 -> toDB <$> Axfr.client serial (IPv6 ip6) zone
    FromFile fn -> loadDB zone fn
  where
    toDB [] = Nothing
    toDB rrs = makeDB zone rrs

----------------------------------------------------------------

findZoneAlist :: Domain -> ZoneAlist -> Maybe (Domain, IORef Zone)
findZoneAlist dom alist = find (\(k, _) -> dom `isSubDomainOf` k) alist

toZoneAlist :: [Zone] -> IO ZoneAlist
toZoneAlist zones = do
    refs <- mapM newIORef zones
    return $ zip names refs
  where
    names = map zoneName zones

newZones :: [ZoneConf] -> IO [Zone]
newZones zcs = mapM newZone zcs

----------------------------------------------------------------

newZone :: ZoneConf -> IO Zone
newZone ZoneConf{..} = do
    mdb <- loadSource zone 0 source
    let (db, ready) = case mdb of
            Nothing -> (emptyDB, False)
            Just db' -> (db', True)
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
            , zoneWakeUp = wakeup
            , zoneWait = wait
            }
  where
    zone = fromRepresentation cnf_zone
    source = readSource cnf_source

shouldReload :: Source -> Bool
shouldReload (FromFile _) = False
shouldReload _ = True

----------------------------------------------------------------

updateZone :: IORef Zone -> IO ()
updateZone zoneref = do
    Zone{..} <- readIORef zoneref
    let serial = soa_serial $ dbSOA zoneDB
    mdb <- loadSource zoneName serial zoneSource
    case mdb of
        Nothing -> return ()
        Just db -> atomicModifyIORef' zoneref $ modify db
  where
    modify db zone = (zone', ())
      where
        zone' =
            zone
                { zoneReady = True
                , zoneDB = db
                }

----------------------------------------------------------------

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

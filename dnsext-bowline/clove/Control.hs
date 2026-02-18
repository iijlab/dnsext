{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Control where

import Data.IORef
import Data.IP
import Data.IP.RouteTable
import Data.Maybe
import Text.Read

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Types

import qualified Axfr
import Config
import Types

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

loadSource :: Serial -> Config -> IO (Maybe DB)
loadSource serial cnf = case readSource $ cnf_source cnf of
    FromUpstream4 ip4 -> toDB <$> Axfr.client serial (IPv4 ip4) dom
    FromUpstream6 ip6 -> toDB <$> Axfr.client serial (IPv6 ip6) dom
    FromFile fn -> loadDB zone fn
  where
    zone = cnf_zone cnf
    dom = fromRepresentation zone
    toDB [] = Nothing
    toDB rrs = makeDB (dom, rrs)

newControl :: Config -> IO (IORef Control)
newControl cnf@Config{..} = do
    mdb <- loadSource 0 cnf
    let (db, ready) = case mdb of
            Nothing -> (emptyDB, False)
            Just db' -> (db', True)
    let (a4, a6) = readIPRange cnf_allow_transfer_addrs
        t4 = fromList $ map (,True) a4
        t6 = fromList $ map (,True) a6
        notify_addrs = readIP cnf_notify_addrs
    newIORef $
        Control
            { ctlDB = db
            , ctlReady = ready
            , ctlNotifyAddrs = notify_addrs
            , ctlAllowTransfer4 = t4
            , ctlAllowTransfer6 = t6
            }

updateControl :: Config -> IORef Control -> IO ()
updateControl cnf ctlref = do
    Control{..} <- readIORef ctlref
    mdb <- loadSource (dbSerial ctlDB) cnf
    case mdb of
        Nothing -> return ()
        Just db -> atomicModifyIORef' ctlref $ modify db
  where
    modify db ctl = (ctl', ())
      where
        ctl' =
            ctl
                { ctlReady = True
                , ctlDB = db
                }

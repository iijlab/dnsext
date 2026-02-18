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

loadSource :: Config -> IO (DB, Bool)
loadSource Config{..} = do
    edb <- loadSource' cnf_zone cnf_source
    case edb of
        Left _ -> return (emptyDB, False)
        Right db' -> return (db', True)

loadSource' :: String -> String -> IO (Either String DB)
loadSource' zone src = case readSource src of
    FromUpstream4 ip4 -> do
        emsg <- Axfr.client (IPv4 ip4) dom
        case emsg of
            Left _e -> return $ Left $ show _e
            Right reply -> return $ makeDB (dom, answer reply)
    FromUpstream6 ip6 -> do
        emsg <- Axfr.client (IPv6 ip6) dom
        case emsg of
            Left _e -> return $ Left $ show _e
            Right reply -> return $ makeDB (dom, answer reply)
    FromFile fn -> loadDB zone fn
  where
    dom = fromRepresentation zone

newControl :: Config -> IO (IORef Control)
newControl cnf@Config{..} = do
    (db, ready) <- loadSource cnf
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
    (db, ready) <- loadSource cnf
    atomicModifyIORef' ctlref $ modify db ready
  where
    modify db ready ctl = (ctl', ())
      where
        ctl' =
            ctl
                { ctlReady = ready
                , ctlDB = db
                }

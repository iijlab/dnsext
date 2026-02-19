{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad
import Data.IP
import Network.Run.TCP.Timeout
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)
import System.Posix (Handler (Catch), installHandler, sigHUP)

import DNS.Auth.Algorithm
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import Data.IORef

import qualified Axfr
import Config
import Net
import Notify
import Types
import Zone

----------------------------------------------------------------

main :: IO ()
main = do
    -- Initialization
    [conffile] <- getArgs
    (Config{..}, zonelist) <- loadConfig conffile
    zones <- newZones zonelist
    zoneAlist <- toZoneAlist zones
    -- Notify
    let (_, zonerefs) = unzip zoneAlist
    _ <- forkIO $ do
        threadDelay 1000000
        mapM_ notifyWithZone zonerefs
    -- Zone updators
    let wakeupAll = sequence_ $ map zoneWakeUp zones
    void $ installHandler sigHUP (Catch wakeupAll) Nothing
    mapM_ (void . forkIO . syncZone) zonerefs
    -- AXFR servers: TCP
    let as = map (axfrServer zoneAlist (show cnf_tcp_port)) cnf_tcp_addrs
    -- Authoritative servers: UDP
    ss <- mapM (serverSocket cnf_udp_port) cnf_udp_addrs
    let cs = map (authServer zoneAlist) ss
    -- Run servers
    foldr1 concurrently_ $ as ++ cs

----------------------------------------------------------------

axfrServer
    :: ZoneAlist
    -> ServiceName
    -> HostName
    -> IO ()
axfrServer zoneAlist port addr =
    runTCPServer 10 (Just addr) port $
        \_ _ s -> Axfr.server zoneAlist s

----------------------------------------------------------------

authServer :: ZoneAlist -> Socket -> IO ()
authServer zoneAlist s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> case opcode query of
                OP_NOTIFY -> handleNotify sa query
                OP_STD -> do
                    let dom = qname $ question query
                    case findZoneAlist dom zoneAlist of -- isSubDomainOf
                        Nothing -> replyRefused s sa query
                        Just (_, zoneref) -> do
                            zone <- readIORef zoneref
                            replyQuery (zoneDB zone) s sa query
                _ -> replyRefused s sa query
        loop
    handleNotify sa query = case lookup dom zoneAlist of -- exact match
        Nothing -> replyRefused s sa query
        Just zoneref -> do
            Zone{..} <- readIORef zoneref
            case fromSockAddr sa of
                Nothing -> replyRefused s sa query
                Just (ip, _)
                    | ip `elem` zoneAllowNotifyAddrs -> do
                        replyNotice s sa query
                        zoneWakeUp
                    | otherwise -> replyRefused s sa query
      where
        dom = qname $ question query

replyNotice :: Socket -> SockAddr -> DNSMessage -> IO ()
replyNotice s sa query = void $ NSB.sendTo s bs sa
  where
    flgs = (flags query){isResponse = True}
    bs = encode $ query{flags = flgs}

replyQuery :: DB -> Socket -> SockAddr -> DNSMessage -> IO ()
replyQuery db s sa query = void $ NSB.sendTo s bs sa
  where
    bs = encode $ getAnswer db query

replyRefused :: Socket -> SockAddr -> DNSMessage -> IO ()
replyRefused s sa query = void $ NSB.sendTo s bs sa
  where
    flgs = (flags query){isResponse = True}
    bs = encode $ query{rcode = Refused, flags = flgs}

----------------------------------------------------------------

syncZone :: IORef Zone -> IO ()
syncZone zoneref = loop
  where
    loop = do
        Zone{..} <- readIORef zoneref
        let tm
                | not zoneShouldRefresh = 0
                | not zoneReady = 10 -- retry
                | otherwise = fromIntegral $ soa_refresh $ dbSOA zoneDB
        zoneWait tm
        -- reading zone source
        updateZone zoneref
        -- notify
        notifyWithZone zoneref
        loop

notifyWithZone :: IORef Zone -> IO ()
notifyWithZone zoneref = do
    Zone{..} <- readIORef zoneref
    mapM_ (notify $ dbZone zoneDB) $ zoneNotifyAddrs

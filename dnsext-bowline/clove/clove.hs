{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.IP
import GHC.Event
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
    [conffile] <- getArgs
    (Config{..}, zonelist) <- loadConfig conffile
    zoneref <- newZone $ head zonelist
    _ <- forkIO $ do
        threadDelay 1000000
        notifyWithZone zoneref
    (wakeup, wait) <- initSync
    void $ installHandler sigHUP (Catch wakeup) Nothing
    _ <- forkIO $ syncZone zoneref wait
    let as = map (axfrServer zoneref (show cnf_tcp_port)) cnf_tcp_addrs
    ais <- mapM (serverResolve cnf_udp_port) cnf_udp_addrs
    ss <- mapM serverSocket ais
    let cs = map (authServer zoneref wakeup) ss
    foldr1 concurrently_ $ as ++ cs

----------------------------------------------------------------

axfrServer
    :: IORef Zone
    -> ServiceName
    -> HostName
    -> IO ()
axfrServer zoneref port addr =
    runTCPServer 10 (Just addr) port $
        \_ _ s -> Axfr.server zoneref s

----------------------------------------------------------------

authServer :: IORef Zone -> IO () -> Socket -> IO ()
authServer zoneref wakeup s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> case opcode query of
                OP_NOTIFY -> do
                    Zone{..} <- readIORef zoneref
                    case fromSockAddr sa of
                        Nothing -> replyRefused s sa query
                        Just (ip, _)
                            | ip `elem` zoneAllowNotifyAddrs -> do
                                replyNotice s sa query
                                wakeup
                            | otherwise -> replyRefused s sa query
                OP_STD -> do
                    zone <- readIORef zoneref
                    replyQuery (zoneDB zone) s sa query
                _ -> do
                    replyRefused s sa query
        loop

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

syncZone :: IORef Zone -> (Int -> IO ()) -> IO ()
syncZone zoneref wait = loop
  where
    loop = do
        Zone{..} <- readIORef zoneref
        let tm
                | not zoneShouldRefresh = 0
                | not zoneReady = 10 -- retry
                | otherwise = fromIntegral $ soa_refresh $ dbSOA zoneDB
        wait tm
        -- reading zone source
        updateZone zoneref
        -- notify
        notifyWithZone zoneref
        loop

notifyWithZone :: IORef Zone -> IO ()
notifyWithZone zoneref = do
    Zone{..} <- readIORef zoneref
    mapM_ (notify $ dbZone zoneDB) $ zoneNotifyAddrs

----------------------------------------------------------------

initSync :: IO (IO (), Int -> IO ())
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

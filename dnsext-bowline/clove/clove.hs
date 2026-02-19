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
import Control
import Net
import Notify
import Types

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    (Config{..}, zonelist) <- loadConfig conffile
    ctlref <- newControl $ head zonelist
    _ <- forkIO $ do
        threadDelay 1000000
        notifyWithControl ctlref
    (wakeup, wait) <- initSync
    void $ installHandler sigHUP (Catch wakeup) Nothing
    _ <- forkIO $ syncZone ctlref wait
    let as = map (axfrServer ctlref (show cnf_tcp_port)) cnf_tcp_addrs
    ais <- mapM (serverResolve cnf_udp_port) cnf_udp_addrs
    ss <- mapM serverSocket ais
    let cs = map (authServer ctlref wakeup) ss
    foldr1 concurrently_ $ as ++ cs

----------------------------------------------------------------

axfrServer
    :: IORef Control
    -> ServiceName
    -> HostName
    -> IO ()
axfrServer ctlref port addr =
    runTCPServer 10 (Just addr) port $
        \_ _ s -> Axfr.server ctlref s

----------------------------------------------------------------

authServer :: IORef Control -> IO () -> Socket -> IO ()
authServer ctlref wakeup s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> case opcode query of
                OP_NOTIFY -> do
                    Control{..} <- readIORef ctlref
                    case fromSockAddr sa of
                        Nothing -> replyRefused s sa query
                        Just (ip, _)
                            | ip `elem` ctlAllowNotifyAddrs -> do
                                replyNotice s sa query
                                wakeup
                            | otherwise -> replyRefused s sa query
                OP_STD -> do
                    ctl <- readIORef ctlref
                    replyQuery (ctlDB ctl) s sa query
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

syncZone :: IORef Control -> (Int -> IO ()) -> IO ()
syncZone ctlref wait = loop
  where
    loop = do
        Control{..} <- readIORef ctlref
        let tm
                | not ctlShouldRefresh = 0
                | not ctlReady = 10 -- retry
                | otherwise = fromIntegral $ soa_refresh $ dbSOA ctlDB
        wait tm
        -- reading zone source
        updateControl ctlref
        -- notify
        notifyWithControl ctlref
        loop

notifyWithControl :: IORef Control -> IO ()
notifyWithControl ctlref = do
    Control{..} <- readIORef ctlref
    mapM_ (notify $ dbZone ctlDB) $ ctlNotifyAddrs

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

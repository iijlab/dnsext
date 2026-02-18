{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
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
import Types

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    cnf@Config{..} <- loadConfig conffile
    ctlref <- newControl cnf
    (wakeup, wait) <- initSync
    void $ installHandler sigHUP (Catch wakeup) Nothing
    _ <- forkIO $ syncZone cnf ctlref wait
    let as = map (axfrServer ctlref (show cnf_tcp_port)) cnf_tcp_addrs
    ais <- mapM (serverResolve cnf_udp_port) cnf_udp_addrs
    ss <- mapM serverSocket ais
    let cs = map (authServer ctlref) ss
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

authServer :: IORef Control -> Socket -> IO ()
authServer ctlref s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> do
                ctl <- readIORef ctlref
                replyQuery (ctlDB ctl) s sa query
        loop

replyQuery :: DB -> Socket -> SockAddr -> DNSMessage -> IO ()
replyQuery db s sa query = void $ NSB.sendTo s bs sa
  where
    bs = encode $ getAnswer db query

----------------------------------------------------------------

syncZone :: Config -> IORef Control -> (Int -> IO ()) -> IO ()
syncZone cnf ctlref wait = loop
  where
    loop = do
        wait 5
        -- reading zone source
        updateControl cnf ctlref
        -- notify
        {-
                Control{..} <- readIORef ctlref
                let addrs = ctlNotifyAddrs
                notify addrs
        -}
        loop

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

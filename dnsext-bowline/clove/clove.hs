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
    (wakeup, wait) <- initSync 5
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

syncZone :: Config -> IORef Control -> IO () -> IO ()
syncZone cnf ctlref wait = loop
  where
    loop = do
        wait
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

initSync :: Int -> IO (IO (), IO ())
initSync refresh = do
    var <- newTVarIO False
    let wakeup = atomically $ writeTVar var True
    if refresh > 0
        then do
            wait <- newWait var wakeup
            return (wakeup, wait)
        else
            return (wakeup, waitBody var)
  where
    newWait var wakeup = do
        tmgr <- getSystemTimerManager
        let register = registerTimeout tmgr (refresh * 1000000) wakeup
            cancel = unregisterTimeout tmgr
        return $ E.bracket register cancel $ \_ -> waitBody var
      where

    waitBody var = atomically $ do
        v <- readTVar var
        check v
        writeTVar var False

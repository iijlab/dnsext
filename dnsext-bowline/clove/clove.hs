{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Network.Run.TCP.Timeout
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)

import DNS.Auth.Algorithm
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import Data.IORef

import qualified Axfr as Axfr
import Config
import Control
import Types

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    cnf@Config{..} <- loadConfig conffile
    ctlref <- newControl cnf
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

serverResolve :: PortNumber -> HostName -> IO AddrInfo
serverResolve pn addr = NE.head <$> getAddrInfo (Just hints) (Just addr) (Just port)
  where
    port = show pn
    hints =
        defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE]
            , addrSocketType = Datagram
            }

serverSocket :: AddrInfo -> IO Socket
serverSocket ai = E.bracketOnError (openSocket ai) close $ \s -> do
    setSocketOption s ReuseAddr 1
    bind s $ addrAddress ai
    return s

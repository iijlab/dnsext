{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Network.Run.TCP.Timeout
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)
import System.Exit

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Axfr
import Config

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    Config{..} <- loadConfig conffile
    edb <- loadDB cnf_zone_name cnf_zone_file
    case edb of
        Left emsg -> die emsg
        Right db -> do
            _ <-
                forkIO $
                    mapConcurrently_
                        (axfrServer db (show cnf_tcp_port))
                        cnf_tcp_addrs
            ais <- mapM (serverResolve cnf_udp_port) cnf_udp_addrs
            ss <- mapM serverSocket ais
            mapConcurrently_ (clove db) ss

----------------------------------------------------------------

axfrServer :: DB -> ServiceName -> HostName -> IO ()
axfrServer db port addr =
    runTCPServer 10 (Just addr) port $
        \_ _ s -> axfr db s

clove :: DB -> Socket -> IO ()
clove db s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> replyQuery db s sa query
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

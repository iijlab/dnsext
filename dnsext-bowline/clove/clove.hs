{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.IP
import Data.IP.RouteTable
import qualified Data.List.NonEmpty as NE
import Network.Run.TCP.Timeout
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)
import System.Exit
import Text.Read

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import Data.IORef

import Axfr
import Config

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    Config{..} <- loadConfig conffile
    edb <- loadDB cnf_zone_name cnf_source
    case edb of
        Left emsg -> die emsg
        Right db -> do
            dbref <- newIORef db
            let (a4, a6) = readIPRange cnf_transfer_addrs
                t4 = fromList $ map (,True) a4
                t6 = fromList $ map (,True) a6
            let as = map (axfrServer dbref t4 t6 (show cnf_tcp_port)) cnf_tcp_addrs
            ais <- mapM (serverResolve cnf_udp_port) cnf_udp_addrs
            ss <- mapM serverSocket ais
            let cs = map (authServer dbref) ss
            foldr1 concurrently_ $ as ++ cs

----------------------------------------------------------------

axfrServer
    :: IORef DB
    -> IPRTable IPv4 Bool
    -> IPRTable IPv6 Bool
    -> ServiceName
    -> HostName
    -> IO ()
axfrServer dbref t4 t6 port addr =
    runTCPServer 10 (Just addr) port $
        \_ _ s -> axfrResponder dbref t4 t6 s

authServer :: IORef DB -> Socket -> IO ()
authServer dbref s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> do
                db <- readIORef dbref
                replyQuery db s sa query
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

readIPRange :: [String] -> ([AddrRange IPv4], [AddrRange IPv6])
readIPRange ss0 = loop id id ss0
  where
    loop b4 b6 [] = (b4 [], b6 [])
    loop b4 b6 (s : ss) = case readMaybe s :: Maybe (AddrRange IPv6) of
        Just a6 -> loop b4 (b6 . (a6 :)) ss
        Nothing -> case readMaybe s :: Maybe (AddrRange IPv4) of
            Just a4 -> loop (b4 . (a4 :)) b6 ss
            Nothing -> loop b4 b6 ss

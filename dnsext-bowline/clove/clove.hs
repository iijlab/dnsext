{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import qualified DNS.ZoneFile as ZF
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment

import Config

type DB = M.Map (Domain, TYPE) ResourceRecord

main :: IO ()
main = do
    [conffile] <- getArgs
    Config{..} <- loadConfig conffile
    rrs <- catMaybes . map fromResource <$> ZF.parseFile cnf_zone_file
    let kvs = map (\r -> ((rrname r, rrtype r), r)) rrs
        m = M.fromList kvs
    ais <- mapM (serverResolve cnf_udp_port) cnf_dns_addrs
    ss <- mapM serverSocket ais
    mapConcurrently_ (clove m) ss

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

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

clove :: DB -> Socket -> IO ()
clove m s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            Left _e -> return ()
            Right query -> case question query of
                [q] -> do
                    let reply = query{flags = defaultResponseDNSFlags}
                    let as = case M.lookup (qname q, qtype q) m of
                            Nothing -> []
                            Just a -> [a]
                        bs' = encode $ reply{answer = as}
                    void $ NSB.sendTo s bs' sa
                _ -> return () -- fixme
        loop

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

----------------------------------------------------------------

type DB = M.Map (Domain, TYPE) ResourceRecord

----------------------------------------------------------------

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

----------------------------------------------------------------

clove :: DB -> Socket -> IO ()
clove m s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> replyQuery m s sa query
        loop

replyQuery :: DB -> Socket -> SockAddr -> DNSMessage -> IO ()
replyQuery m s sa query = do
    bs' <-
        if opcode query == OP_STD
            -- RFC 8906: Sec 3.1.3.1
            --
            -- A non-recursive
            -- server is supposed to respond to recursive
            -- queries as if the Recursion Desired (RD)
            -- bit is not set
            then case question query of
                [Question{..}] -> do
                    let as = case M.lookup (qname, qtype) m of
                            Nothing -> []
                            Just a -> [a]
                    return $ encode $ reply{answer = as}
                -- RFC 9619: "In the DNS, QDCOUNT Is (Usually) One"
                _ -> return $ encode $ reply{rcode = FormatErr}
            -- RFC 8906: Sec 3.1.4
            else return $ encode $ reply{rcode = NotImpl}
    void $ NSB.sendTo s bs' sa
  where
    flgs =
        DNSFlags
            { isResponse = True
            , authAnswer = True
            , trunCation = False
            , -- RFC 1035 Sec 4.1.1 -- just copy
              recDesired = recDesired $ flags query
            , recAvailable = False
            , authenData = False
            , chkDisable = False
            }
    reply = query{flags = flgs}

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

----------------------------------------------------------------

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

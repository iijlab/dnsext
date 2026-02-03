{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.Function (on)
import Data.List (groupBy, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)

import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import qualified DNS.ZoneFile as ZF

import Config

----------------------------------------------------------------

type DB = M.Map Domain RRsets

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    Config{..} <- loadConfig conffile
    let zone = fromRepresentation cnf_zone_name
    rrs <- catMaybes . map fromResource <$> ZF.parseFile' cnf_zone_file zone
    let gs = groupBy ((==) `on` rrname) $ sort rrs
        ks = map (rrname . head) gs
        vs = map makeRRsets gs
    let kvs = zip ks vs
        m = M.fromList kvs
    ais <- mapM (serverResolve cnf_udp_port) cnf_dns_addrs
    ss <- mapM serverSocket ais
    mapConcurrently_ (clove zone m) ss

----------------------------------------------------------------

clove :: Domain -> DB -> Socket -> IO ()
clove zone m s = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> replyQuery zone m s sa query
        loop

replyQuery :: Domain -> DB -> Socket -> SockAddr -> DNSMessage -> IO ()
replyQuery zone m s sa query = void $ NSB.sendTo s bs sa
  where
    bs = encode $ processQuery zone m query

-- RFC 8906: Sec 3.1.3.1
--
-- A non-recursive server is supposed to respond to recursive
-- queries as if the Recursion Desired (RD) bit is not set
processQuery :: Domain -> DB -> DNSMessage -> DNSMessage
processQuery zone m query
    -- RFC 8906: Sec 3.1.4
    | opcode query /= OP_STD = reply{rcode = NotImpl}
    | otherwise = case question query of
        [q]
            | not (qname q `isSubDomainOf` zone) -> reply{rcode = Refused}
            | otherwise -> positiveProcess m q reply
        -- RFC 9619: "In the DNS, QDCOUNT Is (Usually) One"
        _ -> reply{rcode = FormatErr}
  where
    -- RFC 6891: Sec 6.1.1
    ednsH = case ednsHeader query of
        EDNSheader _ -> EDNSheader defaultEDNS
        _ -> NoEDNS
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
    reply = query{flags = flgs, ednsHeader = ednsH}

positiveProcess :: DB -> Question -> DNSMessage -> DNSMessage
positiveProcess m Question{..} reply =
    reply
        { answer = ans
        , authority = auth
        }
  where
    (ans, auth) = case M.lookup qname m of
        Nothing -> ([], [])
        Just x ->
            let ans' = case qtype of
                    A -> rrsetA x
                    AAAA -> rrsetAAAA x
                    NS -> rrsetNS x
                    _ -> filter (\r -> rrtype r == qtype) $ rrsetOthers x
                auth' = if null ans' && qtype /= NS then rrsetNS x else []
             in (ans', auth')

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

data RRsets = RRsets
    { rrsetA :: [ResourceRecord]
    , rrsetAAAA :: [ResourceRecord]
    , rrsetNS :: [ResourceRecord]
    , rrsetOthers :: [ResourceRecord]
    }

makeRRsets :: [ResourceRecord] -> RRsets
makeRRsets rs0 =
    let (as, aaaas, nss, others) = loop id id id id rs0
     in RRsets
            { rrsetA = as
            , rrsetAAAA = aaaas
            , rrsetNS = nss
            , rrsetOthers = others
            }
  where
    loop a b c d [] = (a [], b [], c [], d [])
    loop a b c d (r : rs) = case rrtype r of
        A -> loop (a . (r :)) b c d rs
        AAAA -> loop a (b . (r :)) c d rs
        NS -> loop a b (c . (r :)) d rs
        _ -> loop a b c (d . (r :)) rs

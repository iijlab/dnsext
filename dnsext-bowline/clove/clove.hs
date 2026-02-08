{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.Function (on)
import Data.List (groupBy, nub, partition, sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)

import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import qualified DNS.ZoneFile as ZF

import Config

----------------------------------------------------------------

data DB = DB
    { dbZone :: Domain
    , dbSOA :: ResourceRecord
    , dbMap :: M.Map Domain RRsets
    , dbGlue :: M.Map Domain [ResourceRecord]
    }

-- type DB = M.Map Domain RRsets

----------------------------------------------------------------

main :: IO ()
main = do
    [conffile] <- getArgs
    Config{..} <- loadConfig conffile
    db <- loadDB cnf_zone_name cnf_zone_file
    ais <- mapM (serverResolve cnf_udp_port) cnf_dns_addrs
    ss <- mapM serverSocket ais
    mapConcurrently_ (clove db) ss

unsafeHead :: [a] -> a
unsafeHead (x : _) = x
unsafeHead _ = error "unsafeHead"

----------------------------------------------------------------

loadDB :: String -> FilePath -> IO DB
loadDB zone file = make <$> loadZoneFile zone file

loadZoneFile :: String -> FilePath -> IO (Domain, [ResourceRecord])
loadZoneFile zone file = do
    rrs <- catMaybes . map fromResource <$> ZF.parseFile' file dom
    return (dom, rrs)
  where
    dom = fromRepresentation zone

partition3 :: Domain -> [ResourceRecord] -> ([ResourceRecord], [ResourceRecord], [ResourceRecord])
partition3 dom rrs0 = loop rrs0 [] [] []
  where
    loop [] as ns os = (as, ns, os)
    loop (r : rs) as ns os
        | rrname r `isSubDomainOf` dom =
            if rrtype r == NS && rrname r /= dom
                then loop rs as (r : ns) os
                else loop rs (r : as) ns os
        | otherwise = loop rs as ns (r : os)

makeIsDelegated :: [ResourceRecord] -> (Domain -> Bool)
makeIsDelegated rrs = \dom -> or (map (\f -> f dom) ps)
  where
    s = Set.fromList $ map rrname rrs
    ps = map (\x -> (`isSubDomainOf` x)) $ Set.toList s

make :: (Domain, [ResourceRecord]) -> DB
make (_, []) = error "make: no resource records"
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
make (zone, soa : rrs)
    | rrtype soa /= SOA = error "make: no SOA"
    | otherwise =
        DB
            { dbZone = zone
            , dbSOA = soa
            , dbMap = m
            , dbGlue = g
            }
  where
    -- RFC 9471
    -- In-domain and sibling glues only.
    -- Unrelated glues are ignored.
    (as, ns, _os) = partition3 zone rrs
    isDelegated = makeIsDelegated ns
    (glue, inzone) = partition (\r -> isDelegated (rrname r)) as
    m = makeMap makeRRsets $ [soa] ++ ns ++ inzone
    g = makeMap id glue

makeMap :: ([ResourceRecord] -> v) -> [ResourceRecord] -> M.Map Domain v
makeMap conv rrs = M.fromList kvs
  where
    gs = groupBy ((==) `on` rrname) $ sort rrs
    ks = map (rrname . unsafeHead) gs
    vs = map conv gs
    kvs = zip ks vs

----------------------------------------------------------------

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
    bs = encode $ guardNegative db query

-- RFC 8906: Sec 3.1.3.1
--
-- A non-recursive server is supposed to respond to recursive
-- queries as if the Recursion Desired (RD) bit is not set
guardNegative :: DB -> DNSMessage -> DNSMessage
guardNegative db query
    -- RFC 8906: Sec 3.1.4
    | opcode query /= OP_STD = reply{rcode = NotImpl}
    | not (qname q `isSubDomainOf` dbZone db) = reply{rcode = Refused}
    | otherwise = processPositive db q reply
  where
    q = question query
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
            , -- RFC 1034 Sec 4.3.1
              recAvailable = False
            , authenData = False
            , chkDisable = False
            }
    reply = query{flags = flgs, ednsHeader = ednsH}

processPositive :: DB -> Question -> DNSMessage -> DNSMessage
processPositive db@DB{..} q@Question{..} reply =
    reply
        { answer = ans
        , authority = auth
        , additional = add
        , rcode = code
        }
  where
    (ans, auth, add, code) = case M.lookup qname dbMap of
        Nothing -> findDelegation db q
        Just x ->
            let ans' = case qtype of
                    A -> rrsetA x
                    AAAA -> rrsetAAAA x
                    NS -> rrsetNS x
                    _ -> filter (\r -> rrtype r == qtype) $ rrsetOthers x
                auth' = if null ans' && qtype /= NS then rrsetNS x else []
             in if null ans' && null auth'
                    then
                        ([], [dbSOA], [], NoErr)
                    else
                        let add' = findAdditional db auth'
                         in (ans', auth', add', NoErr)

findDelegation
    :: DB
    -> Question
    -> ([ResourceRecord], [ResourceRecord], [ResourceRecord], RCODE)
findDelegation db@DB{..} Question{..} = loop qname
  where
    loop dom
        | dom == dbZone = ([], [dbSOA], [], NXDomain)
        | otherwise = case unconsDomain dom of
            Nothing -> ([], [dbSOA], [], NXDomain)
            Just (_, dom') -> case M.lookup dom dbMap of
                Nothing -> loop dom'
                Just x ->
                    let auth = rrsetNS x
                     in if null auth
                            then
                                ([], [dbSOA], [], NoErr)
                            else
                                let add = findAdditional db auth
                                 in ([], auth, add, NoErr)

findAdditional :: DB -> [ResourceRecord] -> [ResourceRecord]
findAdditional DB{..} auth' = add'
  where
    ns' = nub $ sort $ catMaybes $ map extractNS auth'
    add' = concat $ map lookupAdd ns'
    lookupAdd dom = case M.lookup dom dbGlue of
        Nothing -> []
        Just rs -> rs
    extractNS rr = case fromRData $ rdata rr of
        Nothing -> Nothing
        Just ns -> Just $ ns_domain ns

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

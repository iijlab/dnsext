{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.Algorithm (
    getAnswer,
) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)

import DNS.Auth.DB
import DNS.Types

-- RFC 8906: Sec 3.1.3.1
--
-- A non-recursive server is supposed to respond to recursive
-- queries as if the Recursion Desired (RD) bit is not set
getAnswer :: DB -> DNSMessage -> DNSMessage
getAnswer db query
    -- RFC 8906: Sec 3.1.4
    | opcode query /= OP_STD = reply{rcode = NotImpl}
    | not (qname q `isSubDomainOf` dbZone db) =
        reply
            { rcode = Refused
            , flags = flgs{authAnswer = False}
            }
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
processPositive db@DB{..} q@Question{..} reply = case M.lookup qname dbAnswer of
    Nothing -> findAuthority db q reply
    Just rs
        -- RFC 8482 Sec 4.1
        -- Answer with a Subset of Available RRsets
        | qtype == ANY -> makeAnswer (take 1 rs) []
        | otherwise -> case filter (\r -> rrtype r == CNAME) rs of
            [] ->
                let ans = filter (\r -> rrtype r == qtype) rs
                    add = if qtype == NS then findAdditional db ans else []
                 in makeAnswer ans add
            [c] | length rs == 1 -> case fromRData $ rdata c of
                Nothing -> error "processPositive: never reached"
                Just cname -> processCNAME db q reply c $ cname_domain cname
            _ -> error "processPositive: multiple CNAMEs"
  where
    -- RFC2308 Sec 2.2 No Data
    makeAnswer [] add = makeReply reply [] [dbSOA] add NoErr True
    makeAnswer ans add = makeReply reply ans [] add NoErr True

-- RFC 1912 Sec 2.4 CNAME records
-- This function does not follow CNAME of CNAME.
processCNAME :: DB -> Question -> DNSMessage -> ResourceRecord -> Domain -> DNSMessage
processCNAME DB{..} Question{..} reply c cname
    | qtype == CNAME = makeReply reply [c] [] add NoErr True
  where
    add
        | cname `isSubDomainOf` dbZone =
            fromMaybe [] $ M.lookup cname dbAdditional
        | otherwise = []
processCNAME DB{..} Question{..} reply c cname = makeReply reply ans [] [] NoErr True
  where
    ans = case M.lookup cname dbAnswer of
        Nothing -> [c]
        Just rs -> [c] ++ filter (\r -> rrtype r == qtype) rs

findAuthority
    :: DB
    -> Question
    -> DNSMessage
    -> DNSMessage
findAuthority db@DB{..} Question{..} reply = loop qname
  where
    loop dom
        | dom == dbZone = makeReply reply [] [dbSOA] [] NXDomain True
        | otherwise = case unconsDomain dom of
            Nothing -> makeReply reply [] [dbSOA] [] NXDomain True
            Just (_, dom') -> case M.lookup dom dbAuthority of
                Nothing -> loop dom'
                Just auth
                    | null auth -> makeReply reply [] [dbSOA] [] NoErr True
                    | otherwise ->
                        let add = findAdditional db auth
                         in makeReply reply [] auth add NoErr False

findAdditional
    :: DB
    -> [ResourceRecord] -- NSs in Answer or Authority
    -> [ResourceRecord]
findAdditional DB{..} rs0 = add
  where
    doms0 = nub $ sort $ catMaybes $ map extractNS rs0
    doms = filter (\d -> d `isSubDomainOf` dbZone) doms0
    add = concat $ map lookupAdd doms
    lookupAdd dom = fromMaybe [] $ M.lookup dom dbAdditional
    extractNS rr = ns_domain <$> fromRData (rdata rr)

makeReply :: DNSMessage -> Answers -> AuthorityRecords -> AdditionalRecords -> RCODE -> Bool -> DNSMessage
makeReply reply ans auth add code aa =
    reply
        { answer = ans
        , authority = auth
        , additional = add
        , rcode = code
        , flags = (flags reply){authAnswer = aa}
        }

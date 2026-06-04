{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.Algorithm (
    getAnswer,
    DB (..),
    fromQuery,
) where

import Data.List (nub, sort)
import Data.Maybe (catMaybes)

import DNS.Auth.DB
import DNS.Types

fromQuery :: DNSMessage -> DNSMessage
fromQuery query =
    query
        { -- Copy identifier
          -- Copy question
          flags = flgs
        , ednsHeader = ednsH
        }
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
            , -- RFC 1034 Sec 4.3.1
              recAvailable = False
            , authenData = False
            , chkDisable = False
            }

-- RFC 8906: Sec 3.1.3.1
--
-- A non-recursive server is supposed to respond to recursive
-- queries as if the Recursion Desired (RD) bit is not set
getAnswer :: DB -> DNSMessage -> DNSMessage
getAnswer db query
    -- RFC 8906: Sec 3.1.4
    | opcode query /= OP_STD = reply{rcode = NotImpl}
    | isResponse (flags query) = reply{rcode = Refused}
    | qtype q `elem` [AXFR, IXFR] = reply{rcode = Refused}
    | not (qname q `isSubDomainOf` dbZone db) =
        reply
            { rcode = Refused
            , flags = (flags reply){authAnswer = False}
            }
    -- RFC 8906 Sec3.1.3.1. Recursive Queries
    -- A non-recursive server is supposed to respond to recursive
    -- queries as if the Recursion Desired (RD) bit is not set.
    | otherwise = processPositive db q reply
  where
    q = question query
    reply = fromQuery query

processPositive :: DB -> Question -> DNSMessage -> DNSMessage
processPositive db@DB{..} q@Question{..} reply = case lookupD qname dbAnswer of
    Nothing -> findAuthority db q reply
    Just idb@IDB{..}
        -- RFC 8482 Sec 4.1
        -- Answer with a Subset of Available RRsets
        | qtype == ANY -> makeAnswer (take 1 idbAll) []
        | otherwise -> case lookupT CNAME idb of
            Nothing ->
                let ans = maybe [] entRRSet $ lookupT qtype idb
                    add = if qtype == NS then findAdditional db ans else []
                 in makeAnswer ans add
            Just ent -> case entRRSet ent of
                [c] | length idbAll == 1 -> case fromRData $ rdata c of
                    Nothing -> makeReply reply [] [] [] ServFail False
                    Just cname -> processCNAME db q reply c $ cname_domain cname
                _ -> makeReply reply [] [] [] ServFail False
  where
    -- RFC2308 Sec 2.2 No Data
    makeAnswer ans add = makeReply reply ans auth add NoErr True
      where
        auth
            | null ans = [dbSOArr]
            | otherwise = []

-- RFC 1912 Sec 2.4 CNAME records
-- This function does not follow CNAME of CNAME.
processCNAME :: DB -> Question -> DNSMessage -> ResourceRecord -> Domain -> DNSMessage
processCNAME DB{..} Question{..} reply c cname
    | qtype == CNAME = makeReply reply [c] [] add NoErr True
  where
    add
        | cname `isSubDomainOf` dbZone =
            maybe [] idbAll $ lookupD cname dbAdditional
        | otherwise = []
processCNAME DB{..} Question{..} reply c cname = makeReply reply ans auth [] code True
  where
    (ans, auth, code)
        | cname `isSubDomainOf` dbZone = case lookupD cname dbAnswer of
            -- RFC 2308 Sec 2.1 Name Error
            Nothing -> ([c], [dbSOArr], NXDomain)
            Just idb ->
                let ans' = maybe [] entRRSet $ lookupT qtype idb
                    -- RFC2308 Sec 2.2 No Data
                    auth'
                        | null ans' = [dbSOArr]
                        | otherwise = []
                 in (c : ans', auth', NoErr)
        | otherwise = ([c], [], NoErr)

findAuthority
    :: DB
    -> Question
    -> DNSMessage
    -> DNSMessage
findAuthority db@DB{..} Question{..} reply = loop qname
  where
    loop dom
        | dom == dbZone = makeReply reply [] [dbSOArr] [] NXDomain True
        | otherwise = case unconsDomain dom of
            Nothing -> makeReply reply [] [dbSOArr] [] NXDomain True
            Just (_, dom') -> case lookupD dom dbAuthority of
                Nothing -> loop dom'
                Just IDB{..}
                    -- For RFC 4592 Sec 2.2.2.Empty Non-terminals
                    | null idbAll -> makeReply reply [] [dbSOArr] [] NoErr True -- fixme
                    | otherwise ->
                        let add = findAdditional db idbAll
                         in makeReply reply [] idbAll add NoErr False

findAdditional
    :: DB
    -> [ResourceRecord] -- NSs in Answer or Authority
    -> [ResourceRecord]
findAdditional DB{..} rs0 = add
  where
    doms0 = nub $ sort $ catMaybes $ map extractNS rs0
    doms = filter (\d -> d `isSubDomainOf` dbZone) doms0
    add = concat $ map lookupAdd doms
    lookupAdd dom = maybe [] idbAll $ lookupD dom dbAdditional
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

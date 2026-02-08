{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.Algorithm (
    getAnswer,
) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

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
processPositive db@DB{..} q@Question{..} reply =
    reply
        { answer = ans
        , authority = auth
        , additional = add
        , rcode = code
        , flags = (flags reply){authAnswer = aa}
        }
  where
    (ans, auth, add, code, aa) = case M.lookup qname dbMap of
        Nothing -> findDelegation db q
        Just x ->
            let ans' = case qtype of
                    A -> rrsetA x
                    AAAA -> rrsetAAAA x
                    NS -> rrsetNS x
                    _ -> filter (\r -> rrtype r == qtype) $ rrsetOthers x
                auth' = if null ans' && qtype /= NS then rrsetNS x else []
             in if null ans'
                    then
                        if null auth'
                            then
                                ([], [dbSOA], [], NoErr, True)
                            else
                                let add' = findAdditional db auth'
                                 in (ans', auth', add', NoErr, False)
                    else
                        if qtype == NS
                            then
                                (ans', [], [], NoErr, False)
                            else
                                (ans', [], [], NoErr, True)

findDelegation
    :: DB
    -> Question
    -> ([ResourceRecord], [ResourceRecord], [ResourceRecord], RCODE, Bool)
findDelegation db@DB{..} Question{..} = loop qname
  where
    loop dom
        | dom == dbZone = ([], [dbSOA], [], NXDomain, True)
        | otherwise = case unconsDomain dom of
            Nothing -> ([], [dbSOA], [], NXDomain, True)
            Just (_, dom') -> case M.lookup dom dbMap of
                Nothing -> loop dom'
                Just x ->
                    let auth = rrsetNS x
                     in if null auth
                            then
                                ([], [dbSOA], [], NoErr, True)
                            else
                                let add = findAdditional db auth
                                 in ([], auth, add, NoErr, False)

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

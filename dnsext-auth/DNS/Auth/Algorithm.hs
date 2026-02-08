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
    (ans, auth, add, code, aa) = case M.lookup qname dbAnswer of
        Nothing -> findDelegation db q
        Just rs ->
            let ans' = filter (\r -> rrtype r == qtype) rs
             in if null ans'
                    then
                        ([], [dbSOA], [], NoErr, True)
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
            Just (_, dom') -> case M.lookup dom dbAuthority of
                Nothing -> loop dom'
                Just auth
                    | null auth -> ([], [dbSOA], [], NoErr, True)
                    | otherwise ->
                        let add = findAdditional db auth
                         in ([], auth, add, NoErr, False)

findAdditional :: DB -> [ResourceRecord] -> [ResourceRecord]
findAdditional DB{..} auth0 = add
  where
    doms = nub $ sort $ catMaybes $ map extractNS auth0
    add = concat $ map lookupAdd doms
    lookupAdd dom = case M.lookup dom dbAdditiona of
        Nothing -> []
        Just rs -> rs
    extractNS rr = case fromRData $ rdata rr of
        Nothing -> Nothing
        Just ns -> Just $ ns_domain ns

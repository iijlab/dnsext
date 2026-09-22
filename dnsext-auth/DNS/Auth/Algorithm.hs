{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.Algorithm (
    getAnswer,
    DB (..),
    dbRD_SOA,
    dbSOArr,
    fromQuery,
) where

import Data.ByteString.Short ()
import Data.List (nub, partition, sort)
import Data.Maybe (catMaybes, isJust)

import DNS.Auth.DB
import DNS.SEC
import DNS.Types

fromQuery :: DNSMessage -> DNSMessage
fromQuery query =
    query
        { -- Copy identifier
          -- Copy question
          flags = flgs
        , ednsHeader = ednsH
        , -- Whatever the query carried in these is the querier's, not
          -- ours to send back.  An ordinary query carries nothing in
          -- them, which is why this went unnoticed; one carrying a TSIG
          -- had its own record echoed at it as though we had signed.
          answer = []
        , authority = []
        , additional = []
        }
  where
    -- RFC 6891: Sec 6.1.1
    ednsH = case ednsHeader query of
        EDNSheader edns ->
            EDNSheader $
                defaultEDNS
                    { -- RFC 3225 Sec 3:  The DO bit of the query MUST
                      -- be copied in the response.
                      ednsDnssecOk = ednsDnssecOk edns
                    }
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
    | ednsVerErr = reply{rcode = BadVers}
    -- RFC 8906 Sec3.1.3.1. Recursive Queries
    -- A non-recursive server is supposed to respond to recursive
    -- queries as if the Recursion Desired (RD) bit is not set.
    | otherwise = process db q dnssecOK reply
  where
    q = question query
    reply = fromQuery query
    (ednsVerErr, dnssecOK) = case ednsHeader query of
        EDNSheader eh -> (ednsVersion eh /= 0, ednsDnssecOk eh)
        _ -> (False, False)

----------------------------------------------------------------

data Accumulator = Accumulator
    { accDO :: Bool
    , accRCODE :: RCODE
    , accAnswer :: Answers
    , accAuthority :: AuthorityRecords
    , accAdditional :: AdditionalRecords
    , accLoopLimit :: Int
    }
    deriving (Eq, Show)

emptyAccumulator :: Accumulator
emptyAccumulator =
    Accumulator
        { accDO = False
        , accRCODE = NoErr
        , accAnswer = []
        , accAuthority = []
        , accAdditional = []
        , accLoopLimit = 10 -- fixme: hard-coding
        }

updateAccumulator :: Accumulator -> [ResourceRecord] -> [ResourceRecord] -> [ResourceRecord] -> RCODE -> Accumulator
updateAccumulator acc ans0 auth0 add0 code =
    acc
        { accRCODE = code
        , accAnswer = ans
        , accAuthority = auth
        , accAdditional = add
        }
  where
    ans
        | null ans0 = accAnswer acc
        | otherwise = accAnswer acc ++ ans0
    auth
        | null auth0 = accAuthority acc
        | otherwise = accAuthority acc ++ auth0
    add
        | null add0 = accAdditional acc
        | otherwise = accAdditional acc ++ add0

----------------------------------------------------------------

--                     RRSIG   NSEC
-- Exist               has     has
-- In-domain NS        not     has
-- In-domain DS        has     has
-- Empty non-terminal  not     not
process :: DB -> Question -> Bool -> DNSMessage -> DNSMessage
process db q@Question{..} dnssecOK reply = case lookupDB qname db of
    -- RFC 2308 Sec 2.1 Name Error
    NonEx ->
        let acc = acc0{accRCODE = NXDomain}
         in makeNegativeReply db qname reply acc Nothing
    Deleg rrs _
        | qtype == DS ->
            let ans = fromRRSetSig dnssecOK (filter (\x -> rrsetsigType x == qtype)) rrs
             in makeReply ans Nothing
        | otherwise -> processDelegation db q acc0 reply rrs
    Exist rrs mwild
        -- RFC 8482 Sec 4.1
        -- Answer with a Subset of Available RRsets
        | qtype == ANY ->
            let ans = fromRRSetSig dnssecOK (take 1) rrs
             in makeReply ans mwild
        | qtype == CNAME ->
            let ans = fromRRSetSig dnssecOK (filter (\x -> rrsetsigType x == qtype)) rrs
             in makeReply ans mwild
        | qtype == NSEC ->
            let ans = lookupN' qname db
             in makeReply ans mwild
        | otherwise -> processCNAME db q qname acc0 reply rrs mwild 0
  where
    acc0 = emptyAccumulator{accDO = dnssecOK}
    makeReply [] mwild = makeNegativeReply db qname reply acc0 mwild
    makeReply ans _ = makePositiveReply reply acc
      where
        acc = updateAccumulator acc0 ans [] [] NoErr

----------------------------------------------------------------

processCNAME :: DB -> Question -> Domain -> Accumulator -> DNSMessage -> [RRSetSig] -> Maybe Domain -> Int -> DNSMessage
processCNAME _ _ _ acc0 reply _ _ cnt
    | cnt >= accLoopLimit acc0 = makePositiveReply reply acc0
processCNAME db q@Question{..} name acc0 reply rrs0 mwild0 cnt = case checkCNAME dnssecOK rrs0 of
    CNErr -> makeErrorReply reply ServFail
    Alias cname cc
        | not (cname `isSubDomainOf` dbZone db) ->
            let acc = updateAccumulator acc0 cc [] [] NoErr
             in makePositiveReply reply acc
        | otherwise -> case lookupDB cname db of
            -- RFC 2308 Sec 2.1 Name Error
            NonEx ->
                let acc = updateAccumulator acc0 cc [] [] NXDomain
                 in makeNegativeReply db cname reply acc Nothing
            Deleg rrs _ ->
                let acc = updateAccumulator acc0 cc [] [] NoErr
                 in processDelegation db q acc reply rrs
            Exist rrs mwild ->
                let acc = updateAccumulator acc0 cc [] [] NoErr
                 in processCNAME db q cname acc reply rrs mwild (cnt + 1)
    Canon ->
        let ans = fromRRSetSig dnssecOK (filter (\x -> rrsetsigType x == qtype)) rrs0
            auth
                | dnssecOK && not (null ans) && isJust mwild0 = case dbNsec3conv db of
                    Nothing ->
                        -- RFC 4035
                        -- Sec 3.1.3.3.  Including NSEC RRs: Wildcard Answer Response
                        lookupN name db
                    Just conv -> case decideNSEC3Proof qname db of
                        -- RFC 5155
                        -- Sec 7.2.6.  Wildcard Answer Responses
                        Nothing -> [] -- never reach
                        Just NSEC3Proof{..} -> lookupN (conv proofNextCloser) db
                | otherwise = []
            add
                | qtype `elem` [NS, MX] = findAdditional db dnssecOK ans
                | otherwise = []
         in if null ans
                -- RFC2308 Sec 2.2 No Data
                then
                    let acc = updateAccumulator acc0 [] [] add NoErr
                     in makeNegativeReply db name reply acc mwild0
                else
                    let acc = updateAccumulator acc0 ans auth add NoErr
                     in makePositiveReply reply acc
  where
    dnssecOK = accDO acc0

----------------------------------------------------------------

processDelegation :: DB -> Question -> Accumulator -> DNSMessage -> [RRSetSig] -> DNSMessage
processDelegation db Question{..} acc0 reply rrs = makePositiveReply reply acc
  where
    dnssecOK = accDO acc0
    allrrs = fromRRSetSig dnssecOK id rrs
    (nss, dss) = partition (\r -> rrtype r == NS) allrrs
    auth
        | not dnssecOK = nss
        | null dss =
            allrrs ++ case dbNsec3conv db of
                -- RFC 4035
                -- Sec 3.1.4.  Including DS RRs in a Response
                Nothing -> lookupN qname db
                -- RFC 5155 Sec 7.2.7: Referrals to Unsigned Subzones
                Just conv -> case decideNSEC3Proof qname db of
                    Nothing -> []
                    Just NSEC3Proof{..} ->
                        unsignedProof (conv closestEncloser) (conv proofNextCloser)
        | otherwise = allrrs
    add = findAdditional db dnssecOK auth
    acc = updateAccumulator acc0 [] auth add NoErr
    -- RFC 5155 Sec 7.2.7: the closest encloser has to be matched and
    -- the next closer covered.  One NSEC3 can do both -- it is the only
    -- one there is where nothing of the zone hashes between them -- and
    -- then it goes in once: an RRset with the same record in it twice
    -- is something RFC 2181 Sec 5 has nobody send, and a resolver
    -- reading the section as RRsets cannot pair the records up with the
    -- signatures.  Which one was found is told by the name it is at,
    -- rather than by comparing records which carry a signature each.
    unsignedProof closest next = case (lookupNSet closest db, lookupNSet next db) of
        (Just x, Just y)
            | rrsetsigName x == rrsetsigName y -> getRRs True x
            | otherwise -> getRRs True x ++ getRRs True y
        (mx, my) -> concatMap (getRRs True) $ catMaybes [mx, my]

----------------------------------------------------------------

findAdditional
    :: DB
    -> Bool
    -> [ResourceRecord] -- NSs in Answer or Authority
    -> [ResourceRecord]
findAdditional db@DB{..} dnssecOK rs0 = add
  where
    doms0 = nub $ sort $ catMaybes (map extractNS rs0) ++ catMaybes (map extractMX rs0)
    doms = filter (\d -> d `isSubDomainOf` dbZone) doms0
    add = concat $ map lookupAdd doms
    aORaaaa = filter (\x -> rrsetsigType x `elem` [A, AAAA])
    lookupAdd dom = fromRRSetSigWith (fromRRSetSig dnssecOK aORaaaa) $ lookupDB dom db
    extractNS rr = ns_domain <$> fromRData (rdata rr)
    extractMX rr = mx_exchange <$> fromRData (rdata rr)

----------------------------------------------------------------

fromRRSetSig :: Bool -> ([RRSetSig] -> [RRSetSig]) -> [RRSetSig] -> [ResourceRecord]
fromRRSetSig dnssecOK f rrs = concat $ map (getRRs dnssecOK) $ f rrs

fromRRSetSigWith :: ([RRSetSig] -> [a]) -> Result -> [a]
fromRRSetSigWith f (Deleg _ (Just rrs)) = f rrs
fromRRSetSigWith f (Exist rrs _) = f rrs
fromRRSetSigWith _ _ = []

----------------------------------------------------------------

makePositiveReply :: DNSMessage -> Accumulator -> DNSMessage
makePositiveReply reply Accumulator{..} =
    reply
        { answer = accAnswer
        , authority = accAuthority
        , additional = accAdditional
        , rcode = accRCODE
        , flags = (flags reply){authAnswer = not (null accAnswer)}
        }

makeNegativeReply :: DB -> Domain -> DNSMessage -> Accumulator -> Maybe Domain -> DNSMessage
makeNegativeReply db dom reply Accumulator{..} mwild =
    reply
        { answer = accAnswer -- CNAME sometime
        -- wildcard may produce duplicated NSEC RRs
        , authority = auth ++ nub (sort proof)
        , additional = accAdditional
        , rcode = accRCODE
        , flags = (flags reply){authAnswer = True}
        }
  where
    auth = dbSOArr accDO db
    proof
        | not accDO = []
        | otherwise = case dbNsec3conv db of
            Nothing -> findNSEC mwild
            Just conv -> findNSEC3 conv mwild

    findNSEC Nothing
        -- RFC 4035
        -- 3.1.3.2.  Including NSEC RRs: Name Error Response
        | accRCODE == NXDomain = case lookupN dom db of
            [] -> []
            xs -> case decideNXWildcard dom db of
                Nothing -> xs
                Just wild -> xs ++ lookupN wild db
        -- RFC 4035
        -- 3.1.3.1.  Including NSEC RRs: No Data Response
        | otherwise = lookupN dom db
    -- RFC 4035
    -- Sec 3.1.3.4.  Including NSEC RRs: Wildcard No Data Response
    findNSEC (Just wild) = lookupN dom db ++ lookupN wild db

    findNSEC3 conv Nothing
        -- RFC 5155
        -- Sec 7.2.2. Name Error Responses
        | accRCODE == NXDomain = case decideNSEC3Proof dom db of
            Nothing -> [] -- never reached
            Just NSEC3Proof{..} ->
                lookupN (conv closestEncloser) db
                    ++ lookupN (conv proofNextCloser) db
                    ++ lookupN (conv proofWildcardCloser) db
        -- RFC 5155
        -- Sec 7.2.3.  No Data Responses, QTYPE is not DS
        --
        -- "dom" exists
        | otherwise = lookupN (conv dom) db
    -- RFC 5155
    -- Sec 7.2.5.  Wildcard No Data Responses
    findNSEC3 conv (Just _wild) = case decideNSEC3Proof dom db of
        Nothing -> [] -- never reached
        Just NSEC3Proof{..} ->
            lookupN (conv closestEncloser) db
                ++ lookupN (conv proofNextCloser) db
                ++ lookupN (conv proofWildcardCloser) db

makeErrorReply :: DNSMessage -> RCODE -> DNSMessage
makeErrorReply reply code =
    reply
        { answer = []
        , authority = []
        , additional = []
        , rcode = code
        , flags = (flags reply){authAnswer = False}
        }

{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.RRset (
    -- * RRset
    encodeRRset,
    sortRDataCanonical,
    canonicalRRset,
    canonicalRRsetSorted,
    canonicalRRsetSortedEither,
) where

-- GHC packages

-- dnsext-types
import DNS.Types
import DNS.Types.Internal

-- this package
import DNS.SEC.Imports
import DNS.SEC.PubAlg
import DNS.SEC.Time (putDNSTime)
import DNS.SEC.Types

putRRSIGHeader :: RD_RRSIG -> Builder ()
putRRSIGHeader RD_RRSIG{..} wbuf ref = do
    put16 wbuf $ fromTYPE rrsig_type
    putPubAlg rrsig_pubalg wbuf ref
    put8 wbuf rrsig_num_labels
    putSeconds rrsig_ttl wbuf ref
    putDNSTime rrsig_expiration wbuf ref
    putDNSTime rrsig_inception wbuf ref
    put16 wbuf rrsig_key_tag
    putDomain Canonical rrsig_zone wbuf ref

{- FOURMOLU_DISABLE -}
sizeRRSIGHeader :: RD_RRSIG -> Int
sizeRRSIGHeader RD_RRSIG{..} =
      2 {- TYPE -}
    + 1 {- PubAlg -}
    + 1 {- num_labels -}
    + 4 {- Seconds -}
    + 4 {- DNSTime -}
    + 4 {- DNSTime -}
    + 2 {- KeyTag -}
    + domainSize rrsig_zone
{- FOURMOLU_ENABLE -}

{- "Reconstructing the Signed Data"
   https://datatracker.ietf.org/doc/html/rfc4035#section-5.3.2
   RR(i) = name | type | class | OrigTTL | RDATA length | RDATA -}
encodeRRset :: RD_RRSIG -> Domain -> TYPE -> CLASS -> [(Int, Builder ())] -> ByteString
encodeRRset rrsig@RD_RRSIG{..} rrset_name rrset_type rrset_class sortedRDatas' = runBuilder sizeStr putRRS
  where
    (rlengths, sortedRDatas) = unzip sortedRDatas'
    putRRH wbuf ref = do
        putDomainRFC1035 Canonical rrset_name wbuf ref
        putTYPE rrset_type wbuf ref
        putCLASS rrset_class wbuf ref
        putSeconds rrsig_ttl wbuf ref
    putRRS wbuf ref = do
        putRRSIGHeader rrsig wbuf ref
        mapM_ (\io -> putRRH wbuf ref >> io wbuf ref) sortedRDatas
    sizeRR rlen =
        domainSize rrset_name
            + 2 {- TYPE -}
            + 2 {- CLASS -}
            + 4 {- seconds -}
            + rlen
    sizeStr = sum $ sizeRRSIGHeader rrsig : map sizeRR rlengths

-- | RFC 4034 Section 6.3: Canonical RR Ordering within an RRset
--   https://datatracker.ietf.org/doc/html/rfc4034#section-6.3
--   "RRs with the same owner name,
--    class, and type are sorted by treating the RDATA portion of the
--    canonical form of each RR as a left-justified unsigned octet sequence"
sortRDataCanonical :: [ResourceRecord] -> [((Int, Builder ()), ResourceRecord)]
sortRDataCanonical rrs =
    {- sortOn "RDATA portion of the canonical form" without RDATA length -}
    map snd $ sortOn fst withWires
  where
    withWires =
        [ (runBuilder sz sput, ((2 {- len size -} + sz, with16Length sput), rr))
        | rr <- rrs
        , let sput = putRData Canonical $ rdata rr
              sz = rdataSize $ rdata rr
        ]

{- FOURMOLU_DISABLE -}
{- assume sorted input. generalized RRset with CPS -}
-- | Checking the sorted RRSet and passing the sorted RRset to the function.
canonicalRRsetSorted
    :: [ResourceRecord]
    -> (String -> a) -> (Domain -> TYPE -> CLASS -> TTL -> [RData] -> a) -> a
canonicalRRsetSorted rrs leftK rightK = either leftK id $ do
    (hd, xs) <- maybe (Left "canonicalRRsetSorted: require non-empty RRset") Right $ uncons rrs
    let eqhd x = ((==) `on` rrname)  hd x  &&
                 ((==) `on` rrtype)  hd x  &&
                 ((==) `on` rrclass) hd x
    unless (all eqhd xs) $
        Left "canonicalRRsetSorted: requires same ( rrname, rrtype, rrclass )"
    let rds = [rdata rr | rr <- rrs]
    unless (all ((== 1) . length) $ group rds) $
        Left "canonicalRRsetSorted: requires unique RData set"
    return $ rightK (rrname hd) (rrtype hd) (rrclass hd) (rrttl hd) rds
{- FOURMOLU_ENABLE -}

-- | Checking the sorted RRSet and returning a continuation.
canonicalRRsetSortedEither
    :: [ResourceRecord]
    -> Either String ((Domain -> TYPE -> CLASS -> TTL -> [RData] -> a) -> a)
canonicalRRsetSortedEither rrs = canonicalRRsetSorted rrs Left (\n ty cls ttl rd -> Right $ \h -> h n ty cls ttl rd)

{- FOURMOLU_DISABLE -}
{- generalized RRset with CPS -}
-- | Sorting RRSet and passing it to the function.
canonicalRRset
    :: [ResourceRecord]
    -> (String -> a)
    -> ((Domain -> TYPE -> CLASS -> TTL -> [RData] -> a) -> a)
canonicalRRset rrs = canonicalRRsetSorted [rr | (_, rr) <- sortRDataCanonical rrs]

{- FOURMOLU_ENABLE -}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.NSECxRange where

-- dnsext-types
import DNS.Types

-- this package
import DNS.SEC.Imports
import DNS.SEC.Types

{- FOURMOLU_DISABLE -}
data Impl range refined = forall bound . Ord bound =>
    Impl
    { nrangeTYPE :: TYPE
    , nrangeTake :: ResourceRecord -> Maybe range
    , nrangeRefine :: Domain -> range -> Either String refined
    , nrangeLower :: refined -> bound
    , nrangeUpper :: refined -> bound
    }
{- FOURMOLU_DISABLE -}

---

zipSigsets :: Impl range refined -> [ResourceRecord] -> (String -> a) -> ([(ResourceRecord, range, [(RD_RRSIG, TTL)])] -> a) -> a
zipSigsets Impl{..} = zipSigsets_ nrangeTYPE nrangeTake

zipSigsets_ :: TYPE -> (ResourceRecord -> Maybe r) -> [ResourceRecord] -> (String -> a) -> ([(ResourceRecord, r, [(RD_RRSIG, TTL)])] -> a) -> a
zipSigsets_ nsecTy takeRange srrs leftK rightK = either leftK rightK $ zipSigs ranges sigsets
  where
    ranges = sortOn (rrname . fst) [(rr, range) | rr <- srrs, rrtype rr == nsecTy, Just range <- [takeRange rr]]
    sigsets = [(rrn, map snd g) | g@((rrn, _) : _) <- groupBy ((==) `on` fst) $ sortOn fst sigs]
    sigs =
        [ (rrname rr, (rd, rrttl rr))
        | rr <- srrs
        , rrtype rr == RRSIG
        , Just rd@RD_RRSIG{..} <- [fromRData $ rdata rr]
        , rrsig_type == nsecTy
        ]
    zipSigs [] [] = Right []
    zipSigs [] ((_, ss) : _) = errorOrphanRRSIG ss
    zipSigs ((rr, _range) : _) [] = errorNoRRSIG rr
    zipSigs ((rr, range) : rs) ((srrn, ss) : sgs) = do
        let rrn = rrname rr
        when (rrn < srrn) $ errorNoRRSIG rr
        when (rrn > srrn) $ errorOrphanRRSIG ss
        zs <- zipSigs rs sgs
        Right $ (rr, range, ss) : zs
    errorNoRRSIG rr = Left $ "NSECx.with-signed: " ++ show nsecTy ++ " without RRSIG found: " ++ show rr
    errorOrphanRRSIG ss = Left $ "NSECx.with-signed: orphan RRSIGs found: " ++ show ss

---

{- | The delegation point as the /parent/ holds it: NS set, SOA clear.
     RFC 6840 Sec 4.1 calls it an ancestor delegation.  The apex of the
     child has both bits, so this does not catch it. -}
ancestorDelegation :: [TYPE] -> Bool
ancestorDelegation types = NS `elem` types && SOA `notElem` types

{- | Why a bitmap at a name cannot prove a name below it is absent:
     an ancestor delegation, or a DNAME, neither of which RFC 6840
     Sec 4.1 lets speak for anything below. -}
encloserRefusal :: String -> [TYPE] -> Maybe String
encloserRefusal tag types
    | ancestorDelegation types = Just $ tag ++ ": ancestor delegation, says nothing below the zone cut"
    | DNAME `elem` types = Just $ tag ++ ": DNAME, says nothing about a subdomain"
    | otherwise = Nothing

{- | Why a bitmap at the name itself cannot prove there is no RRset of
     this type: the type is there, a CNAME is there (RFC 6840 Sec 4.3),
     or it is an ancestor delegation and the type is not the DS. -}
noDataRefusal :: String -> TYPE -> [TYPE] -> Maybe String
noDataRefusal tag qtype types
    | qtype `elem` types = Just $ tag ++ ": type bitmap has query type `" ++ show qtype ++ "`."
    | CNAME `elem` types = Just $ tag ++ ": type bitmap has `CNAME`, so this is not a NODATA."
    | qtype /= DS && ancestorDelegation types =
        Just $ tag ++ ": ancestor delegation, says nothing about `" ++ show qtype ++ "` below the zone cut"
    | otherwise = Nothing

---

uniqueSorted :: Show refined => Impl range refined -> [refined] -> Either String ()
uniqueSorted Impl{..} = uniqueSorted_ nrangeLower nrangeUpper

---

-- $setup
-- >>> :seti -XTypeApplications

-- |
-- util to check unique and ordered range set of NSEC/NSEC3
--
-- >>> import Data.Either (isLeft)
-- >>> uniqueSortedI = uniqueSorted_ @(Int,Int)
-- >>> uniqueSortedI fst snd []
-- Right ()
-- >>> uniqueSortedI fst snd [(1,3)]
-- Right ()
-- >>> uniqueSortedI fst snd [(4,1)]
-- Right ()
-- >>> uniqueSortedI fst snd [(1,3),(3,4),(4,1)]
-- Right ()
-- >>> isLeft $ uniqueSortedI fst snd [(1,3),(2,4),(4,1)]
-- True
-- >>> isLeft $ uniqueSortedI fst snd [(1,3),(3,1),(4,1)]
-- True
uniqueSorted_ :: (Show r, Ord a) => (r -> a) -> (r -> a) -> [r] -> Either String ()
uniqueSorted_ lower upper ranges = case reverse ranges of
    [] -> Right ()
    x : rs
        | not goodOrder -> Left $ "unique-ranges: not good ordered range found: " ++ show notOrdered
        | overlap -> Left $ "unique-ranges: overlapped range found: " ++ show overlapped
        | otherwise -> Right ()
      where
        rotated = lower x > upper x {- only max range is allowed to be rotated -}
        --
        {- checking lower bound and upper bound is ordered -}
        orders = (order x || rotated, x) : map ((,) <$> order <*> id) rs
        notOrdered = map snd $ filter (not . fst) orders
        goodOrder = all fst orders
        --
        {- checking ranges is not overlapped -}
        nexts
            | rotated = us ++ u
            | otherwise = us
        (u, us) = splitAt 1 ranges
        overlaps = [(upper r > lower n, (r, n)) | (r, n) <- zip ranges nexts]
        overlap = any fst overlaps
        overlapped = map snd $ filter fst overlaps
  where
    order r = lower r < upper r

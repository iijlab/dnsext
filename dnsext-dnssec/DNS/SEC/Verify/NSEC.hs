{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.NSEC where

-- ghc packages
import Data.String (fromString)

-- dnsext-types
import DNS.Types hiding (qname)

-- this package
import DNS.SEC.Imports
import DNS.SEC.Types
import qualified DNS.SEC.Verify.NSECxRange as NRange
import DNS.SEC.Verify.Types

rangeImpl :: NRange.Impl NSEC_Range NSEC_Range
rangeImpl =
    NRange.Impl
        { nrangeTYPE = NSEC
        , nrangeTake = takeRange
        , nrangeRefine = refineRange
        , nrangeLower = fst
        , nrangeUpper = nsec_next_domain . snd
        }
  where
    takeRange ResourceRecord{..} = (,) rrname <$> fromRData rdata

refineRange :: Domain -> NSEC_Range -> Either String NSEC_Range
refineRange zone range@(rrn, _rd)
    | rrn `isSubDomainOf` zone = Right range
    | otherwise = Left $ "NSEC.refineRange: not under zone: " ++ show rrn

---

getResult :: Logic a -> Domain -> [NSEC_Range] -> Domain -> Either String a
getResult nlogic zone ranges qname = do
    refine <- nsecRefineWithRanges ranges
    let qnames = refine qname
        coverwild = refine (fromString "*" <> zone)
        noEncloser = Left $ unlines $ "NSEC.getResult: no NSEC encloser:" : ["  " ++ show o ++ " " ++ show rd | (o, rd) <- ranges]
    fromMaybe noEncloser $ nlogic qnames coverwild

type Logic a = [RangeProp] -> [RangeProp] -> Maybe (Either String a)

---

{- FOURMOLU_DISABLE -}
detect :: Domain -> TYPE -> Logic NSEC_Result
detect zone qtype qnames coverwild =
    fmap NSECR_WildcardNoData      <$> get_wildcardNoData           qtype qnames coverwild  <|>
    fmap NSECR_NameError           <$> get_nameError                      qnames coverwild  <|>
    fmap NSECR_UnsignedDelegation  <$> get_unsignedDelegation  zone       qnames coverwild  <|>
    fmap NSECR_NoData              <$> get_noData                   qtype qnames coverwild  <|>
    fmap NSECR_WildcardExpansion   <$> get_wildcardExpansion              qnames coverwild
{- FOURMOLU_ENABLE -}

---

get_nameError :: Logic NSEC_NameError
get_nameError qnames coverwild = do
    c@(Covers ((owner, RD_NSEC{..}), qn)) <- propCover qnames
    w <- propCover coverwild
    {- RFC 6840 Sec 4.1: where the name is under the covering record's
       owner, that owner may not be a delegation or carry a DNAME. -}
    pure $ case [e | qn `isSubDomainOf` owner, Just e <- [NRange.encloserRefusal "NSEC.NameError" nsec_types]] of
        e : _ -> Left e
        [] -> Right $ nsec_NameError c w

get_noData :: TYPE -> Logic NSEC_NoData
get_noData qtype qnames _coverwild = notElemBitmap <$> propMatch qnames
  where
    notElemBitmap m@(Matches ((_, RD_NSEC{..}), _)) =
        maybe (Right $ nsec_NoData m) Left $ NRange.noDataRefusal "NSEC.NoData" qtype nsec_types

get_unsignedDelegation :: Domain -> Logic NSEC_UnsignedDelegation
get_unsignedDelegation zone qnames _coverwild = do
    c@(Covers ((owner, RD_NSEC{..}), qn)) <- propCover qnames
    guard $ owner /= zone {- owner MUST be sub-level, not zone-top -}
    guard $ qn `isSubDomainOf` owner && NS `elem` nsec_types {- super-domain is NS -}
    guard $ DS `notElem` nsec_types {- not signed -}
    pure $ Right $ nsec_UnsignedDelegation c

get_wildcardExpansion :: Logic NSEC_WildcardExpansion
get_wildcardExpansion qnames _coverwild = Right . nsec_WildcardExpansion <$> propCover qnames

get_wildcardNoData :: TYPE -> Logic NSEC_WildcardNoData
get_wildcardNoData qtype qnames _coverwild = do
    c <- propCover qnames
    let notElemBitmap w@(Wilds ((_, RD_NSEC{..}), _)) =
            maybe (Right $ nsec_WildcardNoData c w) Left $
                NRange.noDataRefusal "NSEC.WildcardNoData" qtype nsec_types
    notElemBitmap <$> propWild qnames

---

newtype Matches a = Matches a deriving (Show)

newtype Covers a = Covers a deriving (Show)

newtype Wilds a = Wilds a deriving (Show)

rangeMatches :: NSEC_Range -> Matches NSEC_Witness
rangeMatches r@(owner, _nsec) = Matches (r, owner)

nsec_NameError :: Covers NSEC_Witness -> Covers NSEC_Witness -> NSEC_NameError
nsec_NameError (Covers name) (Covers wildcard) =
    NSEC_NameError name wildcard

nsec_NoData :: Matches NSEC_Witness -> NSEC_NoData
nsec_NoData (Matches name) =
    NSEC_NoData name

nsec_UnsignedDelegation :: Covers NSEC_Witness -> NSEC_UnsignedDelegation
nsec_UnsignedDelegation (Covers name) =
    NSEC_UnsignedDelegation name

nsec_WildcardExpansion :: Covers NSEC_Witness -> NSEC_WildcardExpansion
nsec_WildcardExpansion (Covers name) =
    NSEC_WildcardExpansion name

nsec_WildcardNoData :: Covers NSEC_Witness -> Wilds NSEC_Witness -> NSEC_WildcardNoData
nsec_WildcardNoData (Covers name) (Wilds wildcard) =
    NSEC_WildcardNoData name wildcard

type RangeProp = RangeProp_ NSEC_Witness

---

data RangeProp_ a
    = M (Matches a)
    | C (Covers a)
    | W (Wilds a)
    deriving (Show)

propMatch :: [RangeProp] -> Maybe (Matches NSEC_Witness)
propMatch xs = case [x | M x <- xs] of
    [] -> Nothing
    [x] -> Just x
    _ -> Nothing

propCover :: [RangeProp] -> Maybe (Covers NSEC_Witness)
propCover xs = case [x | C x <- xs] of
    [] -> Nothing
    [x] -> Just x
    _ -> Nothing

propWild :: [RangeProp] -> Maybe (Wilds NSEC_Witness)
propWild xs = case [x | W x <- xs] of
    [] -> Nothing
    [x] -> Just x
    _ -> Nothing

---

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XTypeApplications

-- |
-- >>> nsecCovers @Int 1 5 3
-- True
-- >>> nsecCovers @Int 1 5 0
-- False
-- >>> nsecCovers @Int 1 5 6
-- False
nsecCovers :: Ord a => a -> a -> a -> Bool
nsecCovers lower upper qv = lower < qv && qv < upper

-- |
--   In the last NSEC RR, the next-domain is the zone apex,
--   so lower bound and upper boundaries are rotated.
--   The zone apex is the first NSEC RR in the canonical ordering of the zone.
--
--   https://datatracker.ietf.org/doc/html/rfc4034#section-4.1.1
--   "The value of the Next Domain Name field in the last NSEC record in the zone is the name of
--    the zone apex (the owner name of the zone's SOA RR). This indicates that the owner name of
--    the NSEC RR is the last name in the canonical ordering of the zone."
--
-- >>> nsecCoversR @Int 5 1 0
-- True
-- >>> nsecCoversR @Int 5 1 6
-- True
-- >>> nsecCoversR @Int 5 1 3
-- False
nsecCoversR :: Ord a => a -> a -> a -> Bool
nsecCoversR lower upper qv = qv < upper || lower < qv

nsecRefineWithRanges :: [NSEC_Range] -> Either String (Domain -> [RangeProp])
nsecRefineWithRanges ranges = do
    uniqueSorted ranges
    Right props
  where
    uniqueSorted = NRange.uniqueSorted rangeImpl
    props qname = [prop | (_, refines) <- results, refine <- refines, Just prop <- [refine qname]]
    results =
        [ (rotated, [withRange, withWild])
        | range@(owner, RD_NSEC{..}) <- ranges
        , let next = nsec_next_domain
              rotated = owner > next
              refineWithRange cover qname
                | qname == owner = Just $ M $ Matches (range, qname)
                | cover owner next qname = Just $ C $ Covers (range, qname)
                | otherwise = Nothing
              withRange
                | rotated = refineWithRange nsecCoversR
                | otherwise = refineWithRange nsecCovers
              wildmatch qname w wildsuper
                | w == fromString "*" && qname `isSubDomainOf` wildsuper = Just $ W $ Wilds (range, qname)
                | otherwise = Nothing
              withWild qname = unconsLabels owner Nothing $ wildmatch qname
        ]

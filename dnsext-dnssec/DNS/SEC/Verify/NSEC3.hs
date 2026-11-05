{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.NSEC3 where

-- ghc packages
import Data.ByteString.Short (fromShort)
import Data.String (fromString)

-- dnsext-types
import DNS.Types hiding (qname)
import qualified DNS.Types.Opaque as Opaque

-- this package
import DNS.SEC.Flags (NSEC3_Flag (OptOut))
import DNS.SEC.Imports
import DNS.SEC.Types
import qualified DNS.SEC.Verify.NSECxRange as NRange
import DNS.SEC.Verify.Types

{- FOURMOLU_DISABLE -}
-- | range type guaranteed to yield a lower boundary (hashed-owner) and upper boundary (next-hashed-owner)
data NSEC3_Refined =
    NSEC3_Refined
    { n3range_hashed_owner :: Opaque -- with Base32 decoded
    , n3range_data :: NSEC3_Range
    }
    deriving Show
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
rangeImpl :: NRange.Impl NSEC3_Range NSEC3_Refined
rangeImpl =
    NRange.Impl
    { nrangeTYPE = NSEC3
    , nrangeTake = takeRange
    , nrangeRefine = refineRange
    , nrangeLower = n3range_hashed_owner
    , nrangeUpper = fromNSEC3Next . nsec3_next_hashed_owner_name . snd . n3range_data
    }
  where
    takeRange ResourceRecord{..} = (,) rrname <$> fromRData rdata
{- FOURMOLU_ENABLE -}

refineRange :: Domain -> NSEC3_Range -> Either String NSEC3_Refined
refineRange zone range@(rrn, _rd) = do
    (owner32h, zname) <- unconsLabels rrn (Left "NSEC3.range: owner has no zone-name") (curry Right)
    when (zname /= zone) $ Left $ mismatch zname
    ownerBytes <- either (Left . ("NSEC3.range: " ++)) Right $ Opaque.fromBase32Hex $ fromShort owner32h
    Right $
        NSEC3_Refined
            { n3range_hashed_owner = ownerBytes
            , n3range_data = range
            }
  where
    mismatch zname = "NSEC3.range: owner-zone: " ++ show zname ++ " =/= zone: " ++ show zone

---

type Logic a = (Domain -> [RangeProp]) -> [[RangeProp]] -> Maybe (Either String a)

getResult
    :: Logic a
    -> Domain
    -> [(NSEC3_Range, Hash)]
    -> Domain
    -> Either String a
getResult n3logic zone n3s qname = do
    refine <- n3RefineWithRanges zone n3s
    let subs = zoneSubDomains qname zone
    when (null subs) $ Left $ "NSEC3.getResult: qname: " ++ show qname ++ " is not under zone: " ++ show zone
    let noEncloser = Left $ unlines $ "NSEC3.getResult: no NSEC3 encloser:" : ["  " ++ show o ++ " " ++ show rd | ((o, rd), _) <- n3s]
    fromMaybe noEncloser $ n3logic refine $ map refine subs

---

{- FOURMOLU_DISABLE -}
detect :: TYPE -> Logic NSEC3_Result
detect qtype getPropSet props =
    {- `stepNE` detects UnsignedDelegation case.
        Run this loop before `getNoData` to apply delegation
        for both UnsignedDelegation and NoData properties -}
    n3GetNonExistence (\_ ps -> stepNE ps)                       getPropSet props  <|>
    fmap N3R_NoData             <$> get_noData             qtype getPropSet props  <|>
    fmap N3R_WildcardExpansion  <$> detect_wildcardExpansion     getPropSet props
  where
    stepNE ps =
        fmap N3R_NameError           <$> step_nameError            getPropSet ps  <|>
        fmap N3R_WildcardNoData      <$> step_wildcardNoData qtype getPropSet ps  <|>
        fmap N3R_UnsignedDelegation  <$> step_unsignedDelegation              ps
{- FOURMOLU_ENABLE -}

---

{- find NSEC3 records which covers or matches with qname or super names of qname,
   to recognize non-existence of domain or non-existence of RRset -}

get_nameError :: Logic NSEC3_NameError
get_nameError = n3GetNonExistence step_nameError

{- find just qname matches -}
get_noData :: TYPE -> Logic NSEC3_NoData
get_noData _ _ [] = Just $ Left "NSEC3.NoData: no prop-set"
get_noData qtype _ (exists : _) = notElemBitmap <$> propMatch exists
  where
    notElemBitmap m@(Matches ((_, RD_NSEC3{..}), _)) =
        maybe (Right $ n3_noData m) Left $ NRange.noDataRefusal "NSEC3.NoData" qtype nsec3_types

get_unsignedDelegation :: Logic NSEC3_UnsignedDelegation
get_unsignedDelegation = n3GetNonExistence $ \_ props -> step_unsignedDelegation props

{- loop for not-zipped prop-set list to check last zone-apex domain -}
detect_wildcardExpansion :: Logic NSEC3_WildcardExpansion
detect_wildcardExpansion _ = {- longest result -} msum . map step
  where
    step nexts = Right . n3_wildcardExpansion <$> propCover nexts

{- FOURMOLU_DISABLE -}
get_wildcardExpansion :: Domain -> Logic NSEC3_WildcardExpansion
get_wildcardExpansion ncn getPropSet _props = Right . n3_wildcardExpansion <$> propCover (getPropSet ncn)
{- FOURMOLU_ENABLE -}

get_wildcardNoData :: TYPE -> Logic NSEC3_WildcardNoData
get_wildcardNoData qtype = n3GetNonExistence $ step_wildcardNoData qtype

---

n3GetNonExistence :: ((Domain -> [RangeProp]) -> RangeProps -> Maybe (Either String r)) -> Logic r
n3GetNonExistence neStep getPropSet props = {- longest result -} msum $ map step pps
  where
    step = neStep getPropSet
    {- reuse computed range-props for closest-name and next-closer-name -}
    pps = zip props (drop 1 props)

step_nameError :: (Domain -> [RangeProp]) -> RangeProps -> Maybe (Either String NSEC3_NameError)
step_nameError getPropSet =
    n3StepNonExistence $ \nextCloser closest@(Matches ((_, RD_NSEC3{..}), clname)) -> do
        let wildcardProps = getPropSet (fromString "*" <> clname)
            {- RFC 6840 Sec 4.1: the closest encloser is where the
               name stops existing, and a delegation or a DNAME there
               leaves the rest of it to somebody else. -}
            nameError w =
                maybe (Right $ n3_nameError closest nextCloser w) Left $
                    NRange.encloserRefusal "NSEC3.NameError" nsec3_types
        nameError <$> propCover wildcardProps

step_unsignedDelegation :: RangeProps -> Maybe (Either String NSEC3_UnsignedDelegation)
step_unsignedDelegation =
    n3StepNonExistence $ \nextCloser@(Covers ((_, nextN3), _)) closest -> do
        let unsignedDelegation
                | OptOut `elem` nsec3_flags nextN3 = Right $ n3_unsignedDelegation closest nextCloser
                | otherwise = Left "NSEC3.UnsignedDelegation: No OptOut flag. Not NameErr, wildcard is not matched or covered."
        pure unsignedDelegation

step_wildcardNoData :: TYPE -> (Domain -> [RangeProp]) -> RangeProps -> Maybe (Either String NSEC3_WildcardNoData)
step_wildcardNoData qtype getPropSet =
    n3StepNonExistence $ \nextCloser closest@(Matches (_, clname)) -> do
        let wildcardProps = getPropSet (fromString "*" <> clname)
            notElemBitmap m@(Matches ((_, RD_NSEC3{..}), _)) =
                maybe (Right $ n3_wildcardNoData closest nextCloser m) Left $
                    NRange.noDataRefusal "NSEC3.WildcardNoData" qtype nsec3_types
        notElemBitmap <$> propMatch wildcardProps

{- step to find non-existence of RRset.
   next-closer-name 'cover' for qname and closest-name 'match' for super-name are checked at the same time. -}
n3StepNonExistence
    :: (Covers NSEC3_Witness -> Matches NSEC3_Witness -> Maybe a)
    -> RangeProps
    -> Maybe a
n3StepNonExistence neHandler (nexts, closests) = do
    nextCloser <- propCover nexts
    closest <- propMatch closests
    neHandler nextCloser closest

---

newtype Matches a = Matches a deriving (Show)

newtype Covers a = Covers a deriving (Show)

n3_nameError
    :: Matches NSEC3_Witness
    -> Covers NSEC3_Witness
    -> Covers NSEC3_Witness
    -> NSEC3_NameError
n3_nameError (Matches closest) (Covers next) (Covers wildcard) =
    NSEC3_NameError closest next wildcard

n3_noData :: Matches NSEC3_Witness -> NSEC3_NoData
n3_noData (Matches closest) =
    NSEC3_NoData closest

n3_unsignedDelegation
    :: Matches NSEC3_Witness -> Covers NSEC3_Witness -> NSEC3_UnsignedDelegation
n3_unsignedDelegation (Matches closest) (Covers next) =
    NSEC3_UnsignedDelegation closest next

n3_wildcardExpansion :: Covers NSEC3_Witness -> NSEC3_WildcardExpansion
n3_wildcardExpansion (Covers next) =
    NSEC3_WildcardExpansion next

n3_wildcardNoData
    :: Matches NSEC3_Witness
    -> Covers NSEC3_Witness
    -> Matches NSEC3_Witness
    -> NSEC3_WildcardNoData
n3_wildcardNoData (Matches closest) (Covers next) (Matches wildcard) =
    NSEC3_WildcardNoData closest next wildcard

type RangeProp = RangeProp_ NSEC3_Witness

type RangeProps = ([RangeProp], [RangeProp])

type Hash = Domain -> Opaque

---

data RangeProp_ a
    = M (Matches a)
    | C (Covers a)
    deriving (Show)

propMatch :: [RangeProp] -> Maybe (Matches NSEC3_Witness)
propMatch xs = case [x | M x <- xs] of
    [] -> Nothing
    [x] -> Just x
    _ -> Nothing

propCover :: [RangeProp] -> Maybe (Covers NSEC3_Witness)
propCover xs = case [x | C x <- xs] of
    [] -> Nothing
    [x] -> Just x
    _ -> Nothing

---

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XTypeApplications

-- |
-- >>> n3Covers @Int 1 5 3
-- True
-- >>> n3Covers @Int 1 5 0
-- False
-- >>> n3Covers @Int 1 5 6
-- False
n3Covers :: Ord a => a -> a -> a -> Bool
n3Covers lower upper qv = lower < qv && qv < upper

-- |
--   In the last NSEC3 RR in the zone, lower limit and upper limit are rotated
--   * The lower limit, owner-name is the largest hash value in the zone
--   * The upper limit, next-domain is the smallest hash value in the zone
--
--   https://datatracker.ietf.org/doc/html/rfc5155#section-3.1.7
--   "The value of the Next Hashed Owner Name
--    field in the last NSEC3 RR in the zone is the same as the hashed
--    owner name of the first NSEC3 RR in the zone in hash order."
--
-- >>> n3CoversR @Int 5 1 0
-- True
-- >>> n3CoversR @Int 5 1 6
-- True
-- >>> n3CoversR @Int 5 1 3
-- False
n3CoversR :: Ord a => a -> a -> a -> Bool
n3CoversR lower upper qv = qv < upper || lower < qv

{- get func to compute 'match' or 'cover' with ranges from NSEC3 record-set -}
n3RefineWithRanges
    :: Domain
    -> [(NSEC3_Range, Hash)]
    -> Either String (Domain -> [RangeProp])
n3RefineWithRanges zone ranges0 = do
    ranges <- sequence [(,) <$> refineRange zone range <*> pure hash | (range, hash) <- ranges0]
    uniqueSorted $ map fst ranges
    takeRefines ranges
  where
    uniqueSorted = NRange.uniqueSorted rangeImpl
    takeRefines :: [(NSEC3_Refined, Hash)] -> Either String (Domain -> [RangeProp])
    takeRefines ranges
        | length (filter fst results) > 1 = Left "NSEC3.n3RefineWithRanges: multiple rotated records found."
        | otherwise = Right props
      where
        props qname = [prop | (_, refine) <- results, Just prop <- [refine qname]]
        results =
            [ (rotated, result)
            | (NSEC3_Refined{..}, hash) <- ranges
            , let owner = n3range_hashed_owner
                  rangeData@(_, RD_NSEC3{..}) = n3range_data
                  next = fromNSEC3Next nsec3_next_hashed_owner_name {- binary hashed value, not base32hex -}
                  rotated = owner > next
                  refineWithRange cover qname
                    {- owner and next are decoded range, not base32hex -}
                    | hash qname == owner = Just $ M $ Matches (rangeData, qname)
                    | cover owner next (hash qname) = Just $ C $ Covers (rangeData, qname)
                    | otherwise = Nothing
                  result
                    | rotated = refineWithRange n3CoversR
                    | otherwise = refineWithRange n3Covers
                    {- base32hex does not change hash ordering. https://datatracker.ietf.org/doc/html/rfc5155#section-1.3
                       "Terminology: Hash order:
                        Note that this order is the same as the canonical DNS name order specified in [RFC4034],
                        when the hashed owner names are in base32, encoded with an Extended Hex Alphabet [RFC4648]." -}
            ]

-- |
-- >>> zoneSubDomains "a.b.c.d.e." "c.d.e."
-- ["a.b.c.d.e.","b.c.d.e.","c.d.e."]
-- >>> zoneSubDomains "example.com." "."
-- ["example.com.","com.","."]
-- >>> zoneSubDomains "example.com." "a.example.com."
-- []
zoneSubDomains :: Domain -> Domain -> [Domain]
zoneSubDomains domain zone
    | domain `isSubDomainOf` zone = takeWhile (/= zone) (reverse (superDomains domain) ++ [fromString "."]) ++ [zone]
    | otherwise = []

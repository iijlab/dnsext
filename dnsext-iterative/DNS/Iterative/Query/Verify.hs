{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Query.Verify (
    -- * verified result continuations
    withResult,
    insecureLog,
    bogusError,
    verifyLog,

    -- * case split for RRSIG verification
    cases,
    casesCanoicalize,
    casesVerify,
    withVerifiedRRset,

    -- * RRSIG, sep DNSKEY verification, for tests
    rrWithRRSIG,
    sepDNSKEY,

    -- * NSEC / NSEC3 verification
    NResultK,
    GetNE,
    GetNoDatas,
    getNameError,
    getUnsignedDelegation,
    getWildcardExpansion,
    getNoDatas,

    -- * low-level, NSEC / NSEC3 verification
    GetResult,
    runHandlers,
    mkHandler,
    nsecWithValid,
    nsec3WithValid,
) where

-- GHC packages
import qualified Data.List.NonEmpty as NE

-- other packages

-- dnsext packages
import qualified DNS.Log as Log
import DNS.RRCache (Ranking)
import qualified DNS.RRCache as Cache
import DNS.SEC
import qualified DNS.SEC.Verify as SEC
import DNS.SEC.Verify.Types
import DNS.Types
import qualified DNS.Types as DNS
import qualified DNS.Types.Internal as DNS
import System.Console.ANSI.Types

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.Helpers
import DNS.Iterative.Query.Utils

-- $setup
-- >>> :seti -XOverloadedLists

{- FOURMOLU_DISABLE -}
withResult
    :: MonadContext m
    => TYPE -> (String -> String)
    -> ([a] -> RRset -> m () -> m () -> m b)
    ->  [a] -> RRset -> m () -> m () -> m b
withResult typ modf rightK xs xRRset logK cacheX =
    mayVerifiedRRS noverify cd bogus valid (rrsMayVerified xRRset)
  where
    valid _   = verifyLog (Just Green) (modf $ "verification success - RRGIG of " ++ show typ) >> result
    cd        = verifyLog (Just Yellow) (modf "no verification - check-disabled") >> result
    noverify  = verifyLog (Just Yellow) (modf "no verification - no DS or no DNSKEY avail") >> result
    result    = cacheX >> rightK xs xRRset logK cacheX
    bogus _   = logK >> bogusError (modf $ "verification failed - RRSIG of " ++ show typ)
{- FOURMOLU_ENABLE -}

insecureLog :: MonadEnv m => String -> m ()
insecureLog ~vmsg = verifyLog (Just Yellow) vmsg

{- FOURMOLU_DISABLE -}
bogusError :: MonadContext m => String -> m a
bogusError ~es = getQS lastQuery_ >>= \(_, as) -> getQS aservMessage_ >>= \mmsg ->
    verifyLog (Just Red) es >> throwQuery (ExtraError (ErrorBogus es) as mmsg)
{- FOURMOLU_ENABLE -}

verifyLog :: MonadEnv m => Maybe Color -> String -> m ()
verifyLog ~vcolor ~vmsg = clogLn Log.DEMO vcolor vmsg

{- FOURMOLU_DISABLE -}
-- |
-- null case is no RR for specified type.
-- nc case is not canonical RRset.
-- right case is after verified, with valid or invalid RRset.
cases
    :: MonadEnv m
    => RequestCD
    -> Domain -> [RD_DNSKEY]
    -> (dm -> ([RR], Ranking)) -> dm
    -> Domain  -> TYPE
    -> (RR -> Maybe a)
    -> m b -> (m () -> m b)
    -> ([a] -> RRset -> m () -> m () -> m b)
    -> m b
cases reqCD zone dnskeys getRanked msg rrn rrty h nullK ncK verifiedK =
    withSection getRanked msg $ \srrs rank -> casesCanoicalize srrs rrn rrty h nullK ncK (canonK srrs rank)
  where
    canonK srrs rank fromRDs crrset sortedRDatas =
        casesVerify reqCD dnskeys (rrsigList zone rrn rrty srrs) rank (rrsName crrset) crrset sortedRDatas (verifiedK fromRDs)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
casesCanoicalize
    :: MonadEnv m
    => [RR] -> Domain -> TYPE
    -> (RR -> Maybe a)
    -> m b -> (m () -> m b)
    -> ([a] -> RRset -> [(Int, DNS.Builder ())] -> m b)
    -> m b
casesCanoicalize srrs rrn rrty h nullK ncK0 canonK
    | null xRRs = nullK
    | otherwise = canonicalRRset xRRs (ncK xRRs) (canonK fromRDs)
  where
    ncK rrs s = ncK0 $ logLines Log.DEMO (("not canonical RRset: " ++ s) : map (("\t" ++) . show) rrs)
    (fromRDs, xRRs) = unzip [(x, rr) | rr <- srrs, rrtype rr == rrty, rrname rr == rrn, Just x <- [h rr]]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
casesVerify
    :: MonadEnv m
    => RequestCD
    -> [RD_DNSKEY] -> [(RD_RRSIG, TTL)] -> Ranking
    -> Domain -> RRset -> [(Int, DNS.Builder ())]
    -> (RRset -> m () -> m () -> m b)
    -> m b
casesVerify reqCD dnskeys sigs rank wildcard crrset sortedRDatas rightK0 = do
    now <- liftIO =<< asksEnv currentSeconds_
    withVerifiedRRset reqCD now dnskeys wildcard crrset sortedRDatas sigs verifiedK
  where
    verifiedK vrrset@(RRset dom typ cls minTTL rds sigrds) = rightK0 vrrset logInv cache
      where
        cache = cacheRRset rank dom typ cls minTTL rds sigrds
        logInv = mayVerifiedRRS (pure ()) (pure ()) (logInvalids . lines) (const $ pure ()) $ rrsMayVerified vrrset
        logInvalids  []    = clogLn Log.DEMO (Just Cyan)  "cases: InvalidRRS"
        logInvalids (e:es) = clogLn Log.DEMO (Just Cyan) ("cases: InvalidRRS: " ++ e) *> logLines Log.DEMO es
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
withVerifiedRRset
    :: RequestCD
    -> EpochTime
    -> [RD_DNSKEY]
    -> Domain -> RRset -> [(Int, DNS.Builder ())] -> [(RD_RRSIG, TTL)]
    -> (RRset -> a)
    -> a
withVerifiedRRset reqCD now dnskeys0 wildcard RRset{..} sortedRDatas sigs0 vk =
    vk $ RRset rrsName rrsType rrsClass minTTL rrsRDatas mayVerified
  where
    mayVerified_ NoCheckDisabled  = notValidNoSig
    mayVerified_ CheckDisabled    = notValidCheckDisabled
    {- RFC 2181 Sec 8: what arrived with the top bit set counts as zero,
       and each TTL is taken that way before the smallest is chosen -}
    ttl = receivedTTL rrsTTL
    noverify = (ttl, mayVerified_ reqCD)
    invalid err = (ttl, notValidInvalid err)
    valid goodSigs = (minimum $ ttl : map receivedTTL sigTTLs ++ map fromIntegral expireTTLs, ValidRRS sigrds)
      where
        (sigrds, sigTTLs) = unzip goodSigs
        expireTTLs = [exttl | sig <- sigrds, let exttl = fromDNSTime (rrsig_expiration sig) - now, exttl > 0]

    (minTTL, mayVerified) = rrWithRRSIG' now dnskeys0 wildcard rrsType rrsClass sortedRDatas sigs0 noverify invalid valid
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
rrWithRRSIG
    :: EpochTime -> [RD_DNSKEY] ->  [RR] -> [(RD_RRSIG, a)]
    -> b -> (String -> b) -> ([(RD_RRSIG, a)] -> b) -> b
rrWithRRSIG now dnskeys0 rrs sigs0 noverify left right = canonicalRRset rrs left cn
  where
    cn RRset{..} sortedRDatas = rrWithRRSIG' now dnskeys0 rrsName rrsType rrsClass sortedRDatas sigs0 noverify left right
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
rrWithRRSIG'
    :: EpochTime -> [RD_DNSKEY]
    -> Domain -> TYPE -> CLASS -> [(Int, DNS.Builder ())] -> [(RD_RRSIG, a)]
    -> b -> (String -> b) -> ([(RD_RRSIG, a)] -> b) -> b
rrWithRRSIG' now dnskeys0 name typ cls sortedRDatas sigs0 noverify left right = result
  where
    (supKeys, unsupKeys) = partition (SEC.supportedDNSKEY . fst) [(k, SEC.keyTag k) | k <- dnskeys0]
    (usedKeys, unusedKeys0) = splitAt rejectLimit supKeys
    dnskeySortKey = (,) <$> dnskey_pubalg . fst <*> snd
    keySets = limitSortedGroupBy collisionLimit dnskeySortKey usedKeys  {- limit keys which have collided keytag -}

    (supSigs, unsupSigs) = partition (SEC.supportedRRSIG . fst) sigs0
    (usedSigs, unusedSigs0) = splitAt rejectLimit supSigs
    rrsigSortKey = (,) <$> rrsig_pubalg <*> rrsig_key_tag
    sigSets = limitSortedGroupBy collisionLimit (rrsigSortKey . fst) usedSigs  {- limit sigs which have same keytag -}

    keySigSets = matchSortedGroup dnskeySortKey (rrsigSortKey . fst) keySets sigSets
    unusedKeys = unusedKeys0 ++ [k | MLeft  ks <- keySigSets, k <- ks] ++ unsupKeys
    unusedSigs = unusedSigs0 ++ [s | MRight ss <- keySigSets, s <- ss] ++ unsupSigs

    verify key sigrd = SEC.verifyRRSIGsorted (toDNSTime now) key sigrd name typ cls sortedRDatas
    verifies = take rejectLimit $  {- limit number of verifications -}
        [ (verify keyrd sigrd, key, sig)
        | Match (dnskeys, sigs) <- keySigSets
        , sig@(sigrd, _) <- sigs
        , key@(keyrd, _) <- dnskeys
        ]

    goodSigs = [sig | (Right (), _, sig) <- verifies]

    showSig sigrd = "rrsig: " ++ show sigrd
    showKey key keyTag = "dnskey: " ++ show key ++ " (key_tag: " ++ show keyTag ++ ")"
    showUnusedKeysSigs =
        [showKey key tag | (key, tag) <- take showLimit unusedKeys] ++ [".." | length unusedKeys > showLimit] ++
        [showSig sigrd   | (sigrd, _) <- take showLimit unusedSigs] ++ [".." | length unusedSigs > showLimit]
      where
        showLimit = 5
    verifyErrors =
        [ s
        | (Left em, (key, dnskeyTag), (sigrd, _)) <- verifies
        , s <- ["  error: " ++ em, "    " ++ show sigrd, "    " ++ showKey key dnskeyTag]
        ]
    noValids
        | null verifies    = unlines $ "no-match key-tags or no-supported keys:" : map ("  " ++) showUnusedKeysSigs
        | otherwise        = unlines $ "no good sigs:" : verifyErrors

    result
        | null usedKeys  = noverify {- no way to verify  -}
        | null usedSigs  = left "supported DNSKEYs exist and supported RRSIGs is null" {- dnskeys is not null, but sigs is null -}
        | null goodSigs  = left noValids
        | otherwise      = right goodSigs
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
sepDNSKEY
    :: [RD_DS] -> Domain -> [RD_DNSKEY]
    -> Either String (NonEmpty (RD_DNSKEY, RD_DS))
sepDNSKEY dss0 dom dnskeys0 = do
    let seps = [ (key, ds) | (Right (), key, ds) <- verifies ]
    list (Left "sepkeyDS: no DNSKEY matches with DS") (\s ss -> Right $ s:|ss) seps
  where
    usedkeys = take rejectLimit [(k, SEC.keyTag k) | k <- dnskeys0, SEC.supportedDNSKEY k]
    dnskeySortKey = (,) <$> dnskey_pubalg . fst <*> snd
    keySets = limitSortedGroupBy collisionLimit dnskeySortKey usedkeys

    usedDss = take rejectLimit [ds | ds <- dss0, SEC.supportedDS ds]
    dsSortKey = (,) <$> ds_pubalg <*> ds_key_tag
    dsSets = limitSortedGroupBy collisionLimit dsSortKey usedDss

    keyDsSets = matchSortedGroup dnskeySortKey dsSortKey keySets dsSets
    verifies = take rejectLimit $
        [ (SEC.verifyDS dom key ds, key, ds)
        | Match (dnskeys, dss) <- keyDsSets
        , (key, _) <- dnskeys
        , ds <- dss
        ]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
limitSortedGroupBy :: Ord b => Int -> (a -> b) -> [a] -> [NonEmpty a]
limitSortedGroupBy colLimit sortKey = map (takeNE colLimit) . NE.groupBy ((==) `on` sortKey) . sortOn sortKey
{- FOURMOLU_ENABLE -}

takeNE :: Int -> NonEmpty a -> NonEmpty a
takeNE n (x :| xs) = x :| take n xs

{- FOURMOLU_DISABLE -}
data Match a b
    = MLeft  a
    | Match (a, b)
    | MRight b
    deriving Show

-- |
-- >>> matchSortedGroup id id [[1], [2], [4], [6], [7]] [[1], [3], [4], [5], [7]] :: [Match [Int] [Int]]
-- [Match ([1],[1]),MLeft [2],MRight [3],Match ([4],[4]),MRight [5],MLeft [6],Match ([7],[7])]
-- >>> matchSortedGroup id id [[1], [2], [4], [6], [7], [9]] [[1], [3], [4], [5], [7]] :: [Match [Int] [Int]]
-- [Match ([1],[1]),MLeft [2],MRight [3],Match ([4],[4]),MRight [5],MLeft [6],Match ([7],[7]),MLeft [9]]
-- >>> matchSortedGroup id id [[1], [2], [4], [6], [7]] [[1], [3], [4], [5], [7], [8]] :: [Match [Int] [Int]]
-- [Match ([1],[1]),MLeft [2],MRight [3],Match ([4],[4]),MRight [5],MLeft [6],Match ([7],[7]),MRight [8]]
matchSortedGroup :: Ord a => (k -> a) -> (s -> a) -> [NonEmpty k] -> [NonEmpty s] -> [Match [k] [s]]
matchSortedGroup kx ky = merge (kx . NE.head) (ky . NE.head) left right pair
  where
    left  x    = (MLeft  (NE.toList x) :)
    right y    = (MRight (NE.toList y) :)
    pair  x y  = (Match  (NE.toList x, NE.toList y) :)
{- FOURMOLU_ENABLE -}

rejectLimit :: Int
rejectLimit = 16

collisionLimit :: Int
collisionLimit = 4

---

type NResultK r m a = r -> [RRset] -> m () -> a

{- FOURMOLU_DISABLE -}
type GetNE m msg r1 r2 a
    =  Domain -> [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> Domain
    -> m a -> (String -> m a) -> (String -> m a)
    -> NResultK r1 m (m a) -> NResultK r2 m (m a)
    -> m a

type GetNoDatas m msg r1 r2 r3 r4 a
    =  Domain -> [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> Domain -> TYPE
    -> m a -> (String -> m a) -> (String -> m a)
    -> NResultK r1 m (m a) -> NResultK r2 m (m a) -> NResultK r3 m (m a) -> NResultK r4 m (m a)
    -> m a
{- FOURMOLU_ENABLE -}

getNameError :: MonadEnv m => GetNE m msg NSEC_NameError NSEC3_NameError a
getNameError zone dnskeys getRanked msg qname =
    getWithFallback (\rs -> SEC.nameErrorNSEC zone rs qname) (\rs -> SEC.nameErrorNSEC3 zone rs qname) dnskeys getRanked msg

getUnsignedDelegation :: MonadEnv m => GetNE m msg NSEC_UnsignedDelegation NSEC3_UnsignedDelegation a
getUnsignedDelegation zone dnskeys getRanked msg qname =
    getWithFallback
        (\rs -> SEC.unsignedDelegationNSEC zone rs qname)
        (\rs -> SEC.unsignedDelegationNSEC3 zone rs qname)
        dnskeys
        getRanked
        msg

getWildcardExpansion :: MonadEnv m => Domain -> GetNE m msg NSEC_WildcardExpansion NSEC3_WildcardExpansion a
getWildcardExpansion ncloser zone dnskeys getRanked msg qname =
    getWithFallback
        (\rs -> SEC.wildcardExpansionNSEC zone rs qname)
        (\rs -> SEC.wildcardExpansionNSEC3 zone rs qname ncloser)
        dnskeys
        getRanked
        msg

---

type GetResult range r = [range] -> Either String r

{- FOURMOLU_DISABLE -}
getWithFallback
    :: MonadEnv m
    => GetResult NSEC_Range nsecr -> GetResult NSEC3_Range nsec3r
    -> [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> m a -> (String -> m a) -> (String -> m a)
    -> NResultK nsecr m (m a) -> NResultK nsec3r m (m a)
    -> m a
getWithFallback getNSEC getNSEC3 dnskeys getRanked msg nullK invalidK leftK rightNSEC rightNSEC3 = nsec
  where
    nsec  = nsecWithValid   dnskeys getRanked msg nullNSEC invalidK nsecK
    nullNSEC = nsec3
    nsecK  ranges rrsets doCache =
        runHandlers "cannot handle NSEC:" leftK $
        handle getNSEC   rightNSEC
      where
        handle = mkHandler ranges rrsets doCache

    nsec3 = nsec3WithValid  dnskeys getRanked msg nullK    invalidK nsec3K
    nsec3K ranges rrsets doCache =
        runHandlers "cannot handle NSEC3:" leftK $
        handle getNSEC3  rightNSEC3
      where
        handle = mkHandler ranges rrsets doCache
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
getNoDatas :: MonadEnv m => GetNoDatas m msg NSEC_WildcardNoData NSEC_NoData NSEC3_WildcardNoData NSEC3_NoData a
getNoDatas zone dnskeys getRanked msg qname qtype nullK invalidK leftK rightNSECwild rightNSEC rightNSEC3wild rightNSEC3 = nsec
  where
    nsec  = nsecWithValid   dnskeys getRanked msg nullNSEC invalidK nsecK
    nullNSEC = nsec3
    nsecK  ranges rrsets doCache =
        runHandlers "cannot handle NSEC NoDatas:" leftK $
        handle wildcardNoData rightNSECwild .
        handle noData         rightNSEC
      where
        handle = mkHandler ranges rrsets doCache
        wildcardNoData rs  = SEC.wildcardNoDataNSEC   zone rs qname qtype
        noData         rs  = SEC.noDataNSEC           zone rs qname qtype

    nsec3 = nsec3WithValid  dnskeys getRanked msg nullK    invalidK nsec3K
    nsec3K ranges rrsets doCache =
        runHandlers "cannot handle NSEC3 NoDatas:" leftK $
        handle wildcardNoData rightNSEC3wild .
        handle noData         rightNSEC3
      where
        handle = mkHandler ranges rrsets doCache
        wildcardNoData rs  = SEC.wildcardNoDataNSEC3  zone rs qname qtype
        noData         rs  = SEC.noDataNSEC3          zone rs qname qtype
{- FOURMOLU_ENABLE -}

---

runHandlers :: String -> (String -> a) -> ((String -> a) -> String -> a) -> a
runHandlers header leftK h = h leftK (header ++ ":\n")

{- FOURMOLU_DISABLE -}
mkHandler
    :: [range]
    -> [RRset]
    -> m ()
    ---
    -> GetResult range r
    -> NResultK r m a
    ---
    -> (String -> a)
    -> (String -> a)
mkHandler ranges rrsets doCache getR resultK fallbackK = case getR ranges of
    Right r  -> \_ -> resultK r rrsets doCache
    Left e   -> fallbackK . (++ ("  " ++ e))
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
nsecWithValid
    :: MonadEnv m
    => [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> m a -> (String -> m a)
    -> ([NSEC_Range] -> [RRset] -> m () -> m a)
    -> m a
{- FOURMOLU_ENABLE -}
nsecWithValid = nsecxWithValid SEC.zipSigsNSEC "NSEC"

{- FOURMOLU_DISABLE -}
nsec3WithValid
    :: MonadEnv m
    => [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> m a -> (String -> m a)
    -> ([NSEC3_Range] -> [RRset] -> m () -> m a)
    -> m a
{- FOURMOLU_ENABLE -}
nsec3WithValid = nsecxWithValid SEC.zipSigsNSEC3 "NSEC3"

type WithZippedSigs r a = [RR] -> (String -> a) -> ([(RR, r, [(RD_RRSIG, TTL)])] -> a) -> a

{- FOURMOLU_DISABLE -}
nsecxWithValid
    :: MonadEnv m
    => WithZippedSigs range (m a) -> String
    -> [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> m a -> (String -> m a)
    -> ([range] -> [RRset] -> m () -> m a)
    -> m a
{- FOURMOLU_ENABLE -}
nsecxWithValid withZippedSigs tag dnskeys getRanked msg nullK invalidK validK0 =
    nsecxWithValid' withZippedSigs tag dnskeys getRanked msg nullK ncK invalidK validK
  where
    ncK = invalidK . ("not canonical NSEC/NSEC3, something wrong: " ++)
    validK = uncurry validK0 . unzip

{- FOURMOLU_DISABLE -}
nsecxWithValid'
    :: MonadEnv m
    => WithZippedSigs range (m a) -> String
    -> [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> m a -> (String -> m a) -> (String -> m a)
    -> ([(range, RRset)] -> m () -> m a)
    -> m a
{- FOURMOLU_ENABLE -}
nsecxWithValid' withZippedSigs tag dnskeys getRanked msg nullK ncK invalidK validK =
    nsecxWithRanges withZippedSigs dnskeys getRanked msg nullK ncK runVerified
  where
    runVerified rps doCache
        | valid = validK rps doCache
        | otherwise = invalidK $ unlines notValidErrors
      where
        (_ranges, rrsets) = unzip rps
        valid = all rrsetValid rrsets

        notValidErrors = header : esInvalid ++ esNoSig
        header = tag ++ " verify errors: "
        esInvalid = [ie | set <- rrsets, NotValidRRS (NV_Invalid ie) <- [rrsMayVerified set]]
        esNoSig = "no-sig RRset list:" : ["  " ++ showRRset set | set <- rrsets, NotValidRRS NV_NoSig <- [rrsMayVerified set]]
        showRRset RRset{..} = unwords [show rrsName, show rrsType, show rrsRDatas]

{- FOURMOLU_DISABLE -}
nsecxWithRanges
    :: MonadEnv m
    => WithZippedSigs range (m a)
    -> [RD_DNSKEY]
    -> (msg -> ([RR], Ranking)) -> msg
    -> m a -> (String -> m a)
    -> ([(range, RRset)] -> m () -> m a)
    -> m a
{- FOURMOLU_ENABLE -}
nsecxWithRanges withZippedSigs dnskeys getRanked msg nullK leftK rightK = do
    now <- liftIO =<< asksEnv currentSeconds_
    withSection getRanked msg $ runSection now
  where
    runSection now srrs rank = withZippedSigs srrs leftK $ runSigned now rank

    runSigned _now _rank [] = nullK
    runSigned now rank rs@(_ : _) = either leftK (runVerified rank) $ mapM (verify now) rs

    runVerified rank vs = rightK vs $ doCache rank vs

    {- TODO: interval search psq cache -}
    doCache _rank vs = mapM_ (\(_range, _rrset@(RRset{})) -> pure ()) vs

    verify now (rr, range, sigs) =
        canonicalRRset [rr] Left $ \rrset sortedRDatas ->
            Right $ withVerifiedRRset NoCheckDisabled now dnskeys (rrsName rrset) rrset sortedRDatas sigs ((,) range)

---

{- get not verified canonical RRset -}
canonicalRRset :: [RR] -> (String -> a) -> (RRset -> [(Int, DNS.Builder ())] -> a) -> a
canonicalRRset rrs leftK rightK =
    SEC.canonicalRRsetSorted sortedRRs leftK mkRRset
  where
    mkRRset dom typ cls ttl rds = rightK (RRset dom typ cls ttl rds notValidNoSig) sortedRDatas
    (sortedRDatas, sortedRRs) = unzip $ SEC.sortRDataCanonical rrs

cacheRRset
    :: MonadEnv m
    => Ranking
    -> Domain
    -> TYPE
    -> CLASS
    -> TTL
    -> [RData]
    -> MayVerifiedRRS
    -> m ()
cacheRRset rank dom typ cls ttl rds mv =
    mayVerifiedRRS noSig checkDisalbed (const $ pure ()) valid mv
  where
    noSig = Cache.noSig rds (pure ()) doCache
    checkDisalbed = Cache.checkDisabled rds (pure ()) doCache
    valid sigs = Cache.valid rds sigs (pure ()) doCache
    doCache crs = do
        insertRRSet <- asksEnv insert_
        logLn Log.DEBUG $ "cacheRRset: " ++ show (((dom, typ, cls), ttl), rank) ++ "  " ++ show rds
        liftIO $ insertRRSet (DNS.Question dom typ cls) ttl crs rank

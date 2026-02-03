{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Query.ResolveJust (
    -- * Iteratively search things
    runResolveExact,
    resolveExact,
    runIterative,
    findNegativeTrustAnchor,

    -- * backword compatibility
    runResolveJust,
    resolveJust,

    -- * Root priming things
    refreshRoot,
    rootPriming,
) where

-- GHC packages
import Data.IORef (atomicWriteIORef, readIORef)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

-- other packages

-- dnsext packages
import DNS.Do53.Client (QueryControls (..))
import qualified DNS.Log as Log
import DNS.RRCache (
    rankedAnswer,
    rankedAuthority,
 )
import qualified DNS.RRCache as Cache
import DNS.SEC
import qualified DNS.ThreadStats as TStat
import DNS.Types
import qualified DNS.Types as DNS
import Data.IP (IP)
import System.Console.ANSI.Types

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Query.Cache
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.Delegation
import DNS.Iterative.Query.Helpers
import DNS.Iterative.Query.Random
import qualified DNS.Iterative.Query.StubZone as Stub
import DNS.Iterative.Query.Types
import DNS.Iterative.Query.Utils
import qualified DNS.Iterative.Query.Verify as Verify
import qualified DNS.Iterative.Query.ZoneMap as ZMap

-- import for doctest
import DNS.Iterative.Query.TestEnv

-- $setup
-- >>> :seti -XFlexibleContexts
-- >>> :seti -XFlexibleInstances
-- >>> :seti -XMonadComprehensions
-- >>> :seti -XOverloadedStrings
-- >>> :seti -Wno-incomplete-uni-patterns
-- >>> :seti -Wno-name-shadowing
-- >>> :seti -Wno-orphans
-- >>> import System.IO
-- >>> import Data.IP (IP (..))
-- >>> import qualified DNS.Types.Opaque as Opaque
-- >>> import DNS.SEC
-- >>> DNS.runInitIO addResourceDataForDNSSEC
-- >>> hSetBuffering stdout LineBuffering

-- test env use from doctest
_newTestEnv :: ([String] -> IO ()) -> IO Env
_newTestEnv putLines = newTestEnvNoCache putLines True

-- norec query use from doctest
_testNorec :: MonadIO m => Env -> Bool -> NonEmpty Address -> Domain -> TYPE -> m (Either DNSError DNSMessage)
_testNorec = testNorec

_findConsumed :: [String] -> IO ()
_findConsumed ss
    | any ("consumes not-filled DS:" `isInfixOf`) ss = putStrLn "consume message found"
    | otherwise = pure ()

_noLogging :: [String] -> IO ()
_noLogging = const $ pure ()

---

{-# DEPRECATED runResolveJust "use resolveExact instead of this" #-}
runResolveJust
    :: Env
    -> Domain
    -> TYPE
    -> QueryControls
    -> IO (Either QueryError (DNSMessage, Delegation))
runResolveJust = runResolveExact

-- get the resolved info from the authoritative servers
runResolveExact
    :: Env
    -> Domain
    -> TYPE
    -> QueryControls
    -> IO (Either QueryError (DNSMessage, Delegation))
runResolveExact cxt n typ cd = runDNSQuery (resolveExact n typ) cxt $ queryParamIN n typ cd

{-# DEPRECATED resolveJust "use resolveExact instead of this" #-}
resolveJust :: MonadQuery m => Domain -> TYPE -> m (DNSMessage, Delegation)
resolveJust = resolveExact

-- |
-- use iterative queries to get the final `DNSMessage` and its associated delegation information
-- from the authoritative server.
-- `CNAME`s are not resolved.
resolveExact :: MonadQuery m => Domain -> TYPE -> m (DNSMessage, Delegation)
resolveExact = resolveExactDC 0

{- FOURMOLU_DISABLE -}
resolveExactDC :: MonadQuery m => Int -> Domain -> TYPE -> m (DNSMessage, Delegation)
resolveExactDC dc n typ
    | dc > mdc = do
        logLn Log.WARN $ unwords ["resolve-exact: not sub-level delegation limit exceeded:", show n, show typ]
        failWithCacheOrigName Cache.RankAnswer DNS.ServerFailure
    | otherwise = do
        anchor <- getAnchor
        liftIO $ TStat.eventLog ("iter.rec " ++ show dc ++ " " ++ show n ++ " " ++ show typ)
        (mmsg, nss) <- iterative dc anchor $ DNS.superDomains' (delegationZone anchor) n
        let reuseMsg msg
                | typ == requestDelegationTYPE  = do
                      logLn Log.DEMO $ unwords ["resolve-exact: skip exact query", show n, show typ, "for last no-delegation"]
                      pure (msg, nss)
                | otherwise                     = request nss
        maybe (request nss) reuseMsg mmsg
  where
    mdc = maxNotSublevelDelegation
    getAnchor = do
        stub <- asksEnv stubZones_
        maybe refreshRoot pure $ Stub.lookupStub stub n
    request nss@Delegation{..} = do
        checkEnabled <- getCheckEnabled
        short <- asksEnv shortLog_
        let withDO = checkEnabled && chainedStateDS nss && not (null delegationDNSKEY)
            ainfo sas = ["resolve-exact: query", show n, show typ] ++ [w | short, w <- "to" : [pprAddr sa | sa <- sas]]
        delegationFallbacks dc withDO (logLn Log.DEMO . unwords . ainfo) nss n typ
{- FOURMOLU_ENABLE -}

maxNotSublevelDelegation :: Int
maxNotSublevelDelegation = 16

-- get delegation information after iterative queries
runIterative
    :: Env
    -> Delegation
    -> Domain
    -> QueryControls
    -> IO (Either QueryError Delegation)
runIterative cxt sa n cd = runDNSQuery (snd <$> iterative 0 sa (DNS.superDomains n)) cxt $ queryParamIN n A cd

{- FOURMOLU_DISABLE -}
-- | iterative queries
-- Follow the chain of delegation repeatedly to locate the set of authoritative servers that are expected to provide the final answer
--
-- ----------------------------------------------------------------------------------------------------
-- To resolve a target domain, `A` queries are repeatedly issued to authoritative servers,
-- starting from the top-level domain (TLD) and proceeding down toward the sub-domains.
--
-- The response message from an authoritative server to an `A` query typically includes:
-- + Authority section  : the names (`NS` records) of the next set of authoritative servers.
-- + Additional section : the corresponding addresses (`A` and `AAAA` records) for those names.
--
-- Using this information (delegation information), the resolver continues querying down the domain hierarchy.
-- The initial search domain is the TLD, and the initial set of authoritative servers is the root-servers.
-- ----------------------------------------------------------------------------------------------------
--
-- >>> testIterative dom = do { root <- refreshRoot; iterative 0 root (DNS.superDomains dom) }
-- >>> env <- _newTestEnv _findConsumed
-- >>> runDNSQuery (testIterative "mew.org.") env (queryParamIN "mew.org." A mempty) $> ()  {- fill-action is not called -}
--
-- >>> runDNSQuery (testIterative "arpa.") env (queryParamIN "arpa." NS mempty) $> ()  {- fill-action is called for `ServsChildZone` -}
-- consume message found
--
-- fst: last response message for not delegated last domain
-- snd: delegation for last domain
iterative :: MonadQuery m => Int -> Delegation -> [Domain] -> m (Maybe DNSMessage, Delegation)
iterative _  nss0  []       = pure (Nothing, nss0)
iterative dc nss0 (x : xs)  = do
    checkEnabled <- getCheckEnabled
    {- If NS is not returned, the information of the same NS is used for the child domain. or.jp and ad.jp are examples of this case. -}
    recurse . fmap (mayDelegation nss0 id) =<< step checkEnabled nss0
  where
    recurse (m, nss) = list1 (pure (m, nss)) (iterative dc nss) xs
    --                                       {- sub-level delegation. increase dc only not sub-level case. -}
    name = x

    stepQuery :: MonadQuery m => Bool -> Delegation -> m (DNSMessage, MayDelegation)
    stepQuery checkEnabled nss@Delegation{..} = do
        let zone = delegationZone
            dnskeys = delegationDNSKEY
        {- When the same NS information is inherited from the parent domain, balancing is performed by re-selecting the NS address. -}
        short <- asksEnv shortLog_
        let withDO = checkEnabled && chainedStateDS nss && not (null delegationDNSKEY)
            ainfo sas = ["iterative: query", show name, show A] ++ [w | short, w <- "to" : [pprAddr sa | sa <- sas]]
        {- Use `A` for iterative queries to the authoritative servers during iterative resolution.
           See the following document:
           QNAME Minimisation Examples: https://datatracker.ietf.org/doc/html/rfc9156#section-4 -}
        (msg, _) <- delegationFallbacks dc withDO (logLn Log.DEMO . unwords . ainfo) nss name requestDelegationTYPE
        let withNoDelegation handler = mayDelegation handler (pure . hasDelegation)
            sharedHandler = servsChildZone dc nss name msg
            cacheHandler = cacheNoDelegation nss zone dnskeys name msg
            logDelegation' d = logDelegation d $> d
            handlers md =
                mapM logDelegation'                              =<<
                mapM fillCachedDelegation                        =<< {- fill from cache for fresh NS list -}
                withNoDelegation (cacheHandler $> noDelegation)  =<<
                withNoDelegation sharedHandler md
        (,) msg <$> (handlers =<< delegationWithCache zone dnskeys name msg)
    logDelegation Delegation{..} = do
        let zplogLn lv = logLn lv . (("zone: " ++ show delegationZone ++ ":\n") ++)
        short <- asksEnv shortLog_
        zplogLn Log.DEMO $ ppDelegation short delegationNS

    lookupERR :: MonadQuery m => m (Maybe RCODE)
    lookupERR = fmap fst <$> lookupErrorRCODE name
    withoutMsg md = pure (Nothing, md)
    withERRC rc = case rc of
        NXDomain   -> withoutMsg noDelegation
        ServFail   -> throw' ServerFailure
        FormatErr  -> throw' FormatError
        Refused    -> throw' OperationRefused
        _          -> throw' ServerFailure
      where throw' e = logLn Log.DEMO ("iterative: " ++ show e ++ " with cached RCODE: " ++ show rc) *> throwDnsError e

    step :: MonadQuery m => Bool -> Delegation -> m (Maybe DNSMessage, MayDelegation)
    step checkEnabled nss@Delegation{..} = do
        let notDelegatedMsg (msg, md) = mayDelegation (Just msg, noDelegation) ((,) Nothing . hasDelegation) md
            stepQuery' = notDelegatedMsg <$> stepQuery checkEnabled nss
            getDelegation FreshD  = stepQuery' {- refresh for fresh parent -}
            getDelegation CachedD = lookupERR >>= maybe (lookupDelegation name >>= maybe stepQuery' withoutMsg) withERRC
            fills = mapM (fillsDNSSEC dc nss)
            --                                    {- fill for no A / AAAA cases aginst NS -}
        mapM fills =<< getDelegation delegationFresh
{- FOURMOLU_ENABLE -}

requestDelegationTYPE :: TYPE
requestDelegationTYPE = A

{- Workaround delegation for one authoritative server has both domain zone and sub-domain zone -}
servsChildZone :: MonadQuery m => Int -> Delegation -> Domain -> DNSMessage -> m MayDelegation
servsChildZone dc nss dom msg =
    handleSOA (handleASIG $ pure noDelegation)
  where
    handleSOA fallback = withSection rankedAuthority msg $ \srrs rank -> do
        let soaRRs = rrListWith SOA soaRD dom (\_ rr -> rr) srrs
        reqCD <- asksQP requestCD_
        case soaRRs of
            [] -> fallback
            [_] -> getWorkaround "SOA" >>= verifySOA reqCD
            _ : _ : _ -> multipleSOA rank soaRRs
      where
        soaRD rd = DNS.fromRData rd :: Maybe DNS.RD_SOA
        multipleSOA rank soaRRs = do
            logLn Log.WARN $ "servs-child: " ++ show dom ++ ": multiple SOAs are found:"
            logLn Log.DEMO $ show dom ++ ": multiple SOA: " ++ show soaRRs
            failWithCache dom Cache.ERR IN rank DNS.ServerFailure {- wrong child-zone  -}
        verifySOA reqCD wd
            | null dnskeys = pure $ hasDelegation wd
            | otherwise = Verify.cases reqCD dom dnskeys rankedAuthority msg dom SOA (soaRD . rdata) nullSOA ncSOA withSOA
          where
            dnskeys = delegationDNSKEY wd
            nullSOA = pure noDelegation {- guarded by soaRRs [] case -}
            ncSOA _ncLog = pure noDelegation {- guarded by soaRRs [_] case. single record must be canonical -}
            withSOA = Verify.withResult SOA (\s -> "servs-child: " ++ s ++ ": " ++ show dom) (\_ _ _ _ -> pure $ hasDelegation wd)
    handleASIG fallback = withSection rankedAnswer msg $ \srrs _rank -> do
        let arrsigRRs = rrListWith RRSIG (signedA <=< DNS.fromRData) dom (\_ rr -> rr) srrs
        case arrsigRRs of
            [] -> fallback
            _ : _ -> hasDelegation <$> getWorkaround "A-RRSIG"
      where
        {- Case when apex of cohabited child-zone has A record,
           * with DNSSEC, signed with child-zone apex.
           * without DNSSEC, indistinguishable from the A definition without sub-domain cohabitation -}
        signedA rd@RD_RRSIG{..} = guard (rrsig_type == A && rrsig_zone == dom) $> rd
    getWorkaround tag = do
        logLn Log.DEMO $ "servs-child: workaround: " ++ tag ++ ": " ++ show dom ++ " may be provided with " ++ show (delegationZone nss)
        fillsDNSSEC dc nss (Delegation dom (delegationNS nss) (NotFilledDS ServsChildZone) [] (delegationFresh nss))

fillsDNSSEC :: MonadQuery m => Int -> Delegation -> Delegation -> m Delegation
fillsDNSSEC dc nss d = do
    reqCD <- asksQP requestCD_
    fillsDNSSEC' reqCD dc nss d

{- FOURMOLU_DISABLE -}
fillsDNSSEC' :: MonadQuery m => RequestCD -> Int -> Delegation -> Delegation -> m Delegation
fillsDNSSEC' CheckDisabled   _dc _nss d = pure d
fillsDNSSEC' NoCheckDisabled  dc  nss d = do
    filled@Delegation{..} <- fillDelegationDNSKEY dc =<< fillDelegationDS dc nss d
    when (chainedStateDS filled && null delegationDNSKEY) $ do
        let zone = show delegationZone
        logLn Log.WARN $ "require-ds-and-dnskey: " ++ zone ++ ": DS is 'chained'-state, and DNSKEY is null"
        clogLn Log.DEMO (Just Red) $ zone ++ ": verification error. dangling DS chain. DS exists, and DNSKEY does not exists"
        throwDnsError DNS.ServerFailure
    return filled
{- FOURMOLU_ENABLE -}

getCheckEnabled :: MonadQuery m => m Bool
getCheckEnabled = noCD <$> asksQP requestCD_
  where
    noCD NoCheckDisabled = True
    noCD CheckDisabled = False

-- | Fill DS for delegation info. The result must be `FilledDS` for success query.
--
-- >>> Right dummyKey = Opaque.fromBase64 "dummykey///dummykey///dummykey///dummykey///"
-- >>> dummyDNSKEY = RD_DNSKEY [ZONE] 3 RSASHA256 $ toPubKey RSASHA256 dummyKey
-- >>> Right dummyDS_ = Opaque.fromBase16 "0123456789ABCD0123456789ABCD0123456789ABCD0123456789ABCD"
-- >>> dummyDS = RD_DS 0 RSASHA256 SHA256 dummyDS_
-- >>> withNS2 dom h1 a1 h2 a2 ds = Delegation dom (DEwithA4 h1 (a1:|[]) :| [DEwithA4 h2 (a2:|[])]) ds [dummyDNSKEY] FreshD
-- >>> parent = withNS2 "org." "a0.org.afilias-nst.info." "199.19.56.1" "a2.org.afilias-nst.info." "199.249.112.1" (FilledDS [dummyDS])
-- >>> mkChild ds = withNS2 "mew.org." "ns1.mew.org." "202.238.220.92" "ns2.mew.org." "210.155.141.200" ds
-- >>> isFilled d = case (delegationDS d) of { NotFilledDS {} -> False; FilledDS {} -> True; AnchorSEP {} -> True }
-- >>> env <- _newTestEnv _noLogging
-- >>> runChild child = runDNSQuery (fillDelegationDS 0 parent child) env (queryParamIN "ns1.mew.org." A mempty)
-- >>> fmap isFilled <$> (runChild $ mkChild $ NotFilledDS CachedDelegation)
-- Right True
-- >>> fmap isFilled <$> (runChild $ mkChild $ NotFilledDS ServsChildZone)
-- Right True
-- >>> fmap isFilled <$> (runChild $ mkChild $ FilledDS [])
-- Right True
fillDelegationDS :: MonadQuery m => Int -> Delegation -> Delegation -> m Delegation
fillDelegationDS dc src dest
    | null $ delegationDNSKEY src = fill [] {- no src DNSKEY, not chained -}
    | NotFilledDS o <- delegationDS src = do
        logLn Log.WARN $ "require-ds: not consumed not-filled DS: case=" ++ show o ++ " zone: " ++ show (delegationZone src)
        return dest
    | FilledDS [] <- delegationDS src = fill [] {- no src DS, not chained -}
    | Delegation{..} <- dest = case delegationDS of
        AnchorSEP{} -> pure dest {- specified trust-anchor dnskey case -}
        FilledDS _ -> pure dest {- no DS or exist DS, anyway filled DS -}
        NotFilledDS o -> do
            logLn Log.DEMO $ "require-ds: consumes not-filled DS: case=" ++ show o ++ " zone: " ++ show delegationZone
            maybe query fill =<< lookupDS delegationZone
  where
    dsRDs (rrs, _rank) = Just [rd | rr <- rrs, Just rd <- [DNS.fromRData $ rdata rr]]
    lookupDS :: MonadQuery m => Domain -> m (Maybe [RD_DS])
    lookupDS zone = lookupValidRR "require-ds" zone DS <&> (>>= dsRDs)
    fill dss = pure dest{delegationDS = FilledDS dss}
    query = fillCachedDelegation =<< fill =<< queryDS dc src (delegationZone dest)

{- FOURMOLU_DISABLE -}
queryDS :: MonadQuery m => Int -> Delegation -> Domain -> m [RD_DS]
queryDS dc src@Delegation{..} dom = do
    short <- asksEnv shortLog_
    let ainfo sas = ["require-ds: query", show zone, show DS] ++ [w | short, w <- "to" : [pprAddr sa | sa <- sas]]
    (msg, _) <- delegationFallbacks dc True (logLn Log.DEMO . unwords . ainfo) src dom DS
    Verify.cases NoCheckDisabled zone dnskeys rankedAnswer msg dom DS (DNS.fromRData . rdata) nullDS ncDS withDS
  where
    nullDS = insecure "no DS, so no verify" $> []
    ncDS ncLog = ncLog *> bogus "not canonical DS"
    withDS rds = Verify.withResult DS msgf (\_ _ _ _ -> pure rds) rds  {- not reach for no-verify and check-disabled cases -}
    insecure ~vmsg = Verify.insecureLog (msgf vmsg)
    bogus ~es = Verify.bogusError (msgf es)
    msgf s = "fill delegation - " ++ s ++ ": " ++ domTraceMsg
    domTraceMsg = show zone ++ " -> " ++ show dom
    zone = delegationZone
    dnskeys = delegationDNSKEY
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
refreshRoot :: MonadQuery m => m Delegation
refreshRoot = do
    curRef <- asksEnv currentRoot_
    let refresh = do
            n <- getRoot
            liftIO $ atomicWriteIORef curRef $ Just n{delegationFresh = CachedD} {- got from IORef as cached -}
            return n
        keep = do
            current <- liftIO $ readIORef curRef
            maybe refresh return current
        checkLife = do
            nsc <- lookupRR "." NS
            maybe refresh (const keep) nsc
    checkLife
  where
    getRoot = do
        let fallback s = do
                {- fallback to rootHint -}
                logLn Log.WARN $ "refreshRoot: " ++ s
                asksEnv rootHint_
        either fallback return =<< rootPriming
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
{-
steps of root priming
1. get DNSKEY RRset of root-domain using `fillDelegationDNSKEY` steps
2. query NS from root-domain with DO flag - get NS RRset and RRSIG
3. verify NS RRset of root-domain with RRSIGa
 -}
rootPriming :: MonadQuery m => m (Either String Delegation)
rootPriming =
    priming =<< fillDelegationDNSKEY 0 =<< getHint
  where
    left s = Left $ "root-priming: " ++ s
    logResult delegationNS color s = do
        clogLn Log.DEMO (Just color) $ "root-priming: " ++ s
        short <- asksEnv shortLog_
        logLn Log.DEMO $ ppDelegation short delegationNS
    nullNS = pure $ left "no NS RRs"
    ncNS _ncLog = pure $ left "not canonical NS RRs"
    pairNS rr = (,) <$> rdata rr `DNS.rdataField` DNS.ns_domain <*> pure rr

    verify hint msgNS = Verify.cases NoCheckDisabled "." dnskeys rankedAnswer msgNS "." NS pairNS nullNS ncNS $
        \nsps nsRRset logK cacheK -> do
            let nsSet = Set.fromList $ map fst nsps
                (axRRs, cacheAX) = withSection Cache.rankedAdditional msgNS $ \rrs rank ->
                    (axList False (`Set.member` nsSet) (\_ rr -> rr) rrs, cacheSection axRRs rank)
                result "."  ents
                    | rrsetValid nsRRset       = do
                          logK *> cacheK *> cacheAX
                          logResult ents Green "verification success - RRSIG of NS: \".\""
                          pure $ Right hint{delegationNS = ents, delegationFresh = FreshD}
                    | FilledDS [] <- anchor    = do
                          logK *> cacheK *> cacheAX
                          logResult ents Yellow "trust anchor disabled, no verification"
                          pure $ Right hint{delegationNS = ents, delegationFresh = FreshD}
                    | otherwise                = do
                          logK {- Call action for logging error info. -}
                          logResult ents Red "verification failed - RRSIG of NS: \".\"" $> left "DNSSEC verification failed"
                result apex _ents = pure $ left $ "inconsistent zone apex: " ++ show apex ++ ", not \".\""
            fromMaybe (pure $ left "no delegation") $ findDelegation' result nsps axRRs
      where
        anchor = delegationDS hint
        dnskeys = delegationDNSKEY hint

    getHint = do
        hint <- asksEnv rootHint_
        anchor <- asksEnv rootAnchor_
        pure hint{delegationDS = anchor}
    priming hint = do
        let short = False
        let zone = "."
            ainfo sas = ["root-priming: query", show zone, show NS] ++ [w | short, w <- "to" : [pprAddr sa | sa <- sas]]
        (msgNS, _) <- delegationFallbacks 0 True (logLn Log.DEMO . unwords . ainfo) hint zone NS
        verify hint msgNS
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
fillDelegationDNSKEY :: MonadQuery m => Int -> Delegation -> m Delegation
fillDelegationDNSKEY _dc d@Delegation{delegationDS = NotFilledDS o, delegationZone = zone} = do
    {- DS(Delegation Signer) is not filled -}
    logLn Log.WARN $ "require-dnskey: not consumed not-filled DS: case=" ++ show o ++ " zone: " ++ show zone
    pure d
fillDelegationDNSKEY _dc d@Delegation{delegationDS = FilledDS []} = pure d {- DS(Delegation Signer) does not exist -}
fillDelegationDNSKEY  dc d@Delegation{..} = fillDelegationDNSKEY' getSEP dc d
  where
    zone = delegationZone
    getSEP = case delegationDS of
        AnchorSEP _ sep     -> \_ -> Right sep
        FilledDS dss@(_:_)  -> (fmap fst <$>) . Verify.sepDNSKEY dss zone . rrListWith DNSKEY DNS.fromRData zone const
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
fillDelegationDNSKEY' :: MonadQuery m => ([RR] -> Either String (NonEmpty RD_DNSKEY)) -> Int -> Delegation -> m Delegation
fillDelegationDNSKEY' _      _dc d@Delegation{delegationDNSKEY = _:_}     = pure d
fillDelegationDNSKEY' getSEP  dc d@Delegation{delegationDNSKEY = [] , ..} =
    maybe query (fill d . toDNSKEYs) =<< lookupValidRR "require-dnskey" zone DNSKEY
  where
    zone = delegationZone
    toDNSKEYs (rrs, _rank) = [rd | rr <- rrs, Just rd <- [DNS.fromRData $ rdata rr]]
    fill d' dnskeys = pure d'{delegationDNSKEY = dnskeys}
    query = cachedDNSKEY getSEP dc d >>= \(ks, d') -> fill d' ks
{- FOURMOLU_ENABLE -}

{-
steps to get verified and cached DNSKEY RRset
1. query DNSKEY from delegatee with DO flag - get DNSKEY RRset and its RRSIG
2. verify SEP DNSKEY of delegatee with DS
3. verify DNSKEY RRset of delegatee with RRSIG
4. cache DNSKEY RRset with RRSIG when validation passes
 -}
cachedDNSKEY
    :: MonadQuery m => ([RR] -> Either String (NonEmpty RD_DNSKEY)) -> Int -> Delegation -> m ([RD_DNSKEY], Delegation)
cachedDNSKEY getSEPs dc d@Delegation{..} = do
    short <- asksEnv shortLog_
    let ainfo sas = ["require-dnskey: query", show zone, show DNSKEY] ++ [w | short, w <- "to" : [pprAddr sa | sa <- sas]]
    (msg, d') <- delegationFallbacks dc True (logLn Log.DEMO . unwords . ainfo) d zone DNSKEY
    let rcode = DNS.rcode msg
    case rcode of
        DNS.NoErr -> withSection rankedAnswer msg $ \srrs _rank ->
            either bogus (fmap (\ks -> (ks, d')) . verifyDNSKEY msg) $ getSEPs srrs
        _ -> bogus $ "error rcode to get DNSKEY: " ++ show rcode
  where
    verifyDNSKEY msg (s :| ss) = do
        let dnskeyRD rr = DNS.fromRData $ rdata rr :: Maybe RD_DNSKEY
            {- no DNSKEY case -}
            nullDNSKEY = cacheSectionNegative zone [] zone DNSKEY rankedAnswer msg [] *> bogus "null DNSKEYs for non-empty SEP"
            ncDNSKEY ncLog = ncLog >> bogus "not canonical"
        Verify.cases NoCheckDisabled zone (s : ss) rankedAnswer msg zone DNSKEY dnskeyRD nullDNSKEY ncDNSKEY withDNSKEY
    withDNSKEY rds = Verify.withResult DNSKEY msgf (\_ _ _ _ -> pure rds) rds {- not reach for no-verify and check-disabled cases -}
    bogus ~es = Verify.bogusError (msgf es)
    msgf s = "require-dnskey: " ++ s ++ ": " ++ show zone
    zone = delegationZone

---

{- FOURMOLU_DISABLE -}
delegationFallbacks
    :: MonadQuery m
    => Int -> Bool -> ([Address] -> m b)
    -> Delegation -> Domain -> TYPE -> m (DNSMessage, Delegation)
delegationFallbacks dc dnssecOK ah d0 name typ = do
    eventLog' "bgn"
    disableV6NS <- asksEnv disableV6NS_
    x <- delegationFallbacks_ handled failed qparallel disableV6NS dc dnssecOK ah d0 name typ
    eventLog' "end"
    pure x
  where
    handled = logLn Log.DEMO
    failed ass = logLines Log.DEMO ("delegationFallbacks: failed:" : ["  " ++ unwords (ns : map pprAddr as) | (ns, as) <- ass])
    qparallel = 2
    eventLog' s = asksQP origQuestion_ >>= liftIO . putLog
        where putLog (Question n ty _) = TStat.eventLog $ unwords ["iter.dfb", s, show name, show typ, show dc, "orig:", show n, show ty]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- |
--
-- >>> fallbacks = delegationFallbacks_ (const $ pure ()) (const $ pure ()) 2 True 0 True (const $ pure ())
-- >>> --
-- >>> foldIP0 ns = foldIPList' (DEonlyNS ns:|[]) (\v4 -> DEwithA4 ns v4:|[]) (\v6 -> DEwithA6 ns v6:|[]) (\v4 v6 -> DEwithAx ns v4 v6:|[])
-- >>> foldIP ns ips = foldIP0 ns [i | IPv4 i <- ips] [i | IPv6 i <- ips]
-- >>> nsList nss = [de | (ns, axs) <- nss, de <- foldIP ns axs]
-- >>> delegation zone nss = Delegation{delegationZone=zone, delegationNS=nsList nss, delegationDS=FilledDS [], delegationDNSKEY=[], delegationFresh=CachedD}
-- >>> --
-- >>> env <- _newTestEnv $ \_ -> pure ()
-- >>> qparam dom typ = queryParamIN dom typ mempty
-- >>> --
-- >>> type Case = (IP, Domain, TYPE, Either DNSError [RData])
-- >>> type TestIO = QueryT (ReaderT [Case] IO)
-- >>> qnorec dok as name typ = do { env <- asksEnv id ; _testNorec env dok as name typ }
-- >>> rdRRs name typ rds = [ResourceRecord{rrname = name, rrtype = typ, rrclass = IN, rrttl = 600, rdata = rd} | rd <- rds]
-- >>> dnsMsg name typ rds = defaultResponse{answer = rdRRs name typ rds}
-- >>> lkCase0    aas name typ cs = [r | (ip, nm, ty, r) <- cs, let a:|as = aas, (ia, _) <- a:as, nm == name, ty == typ, ip == ia]
-- >>> lkCase dok as  name typ cs = case [r | r@Right{} <- xs] ++ [l | l@Left{} <- xs] of { [] -> qnorec dok as name typ; x:_ -> pure $ dnsMsg name typ <$> x } where xs = lkCase0 as name typ cs
-- >>> qlookup dok as name typ = do { cs <- lift $ lift $ lift $ lift ask ; lkCase dok as name typ cs }
-- >>> instance MonadQuery TestIO where { queryNorec = qlookup }
-- >>> --
-- >>> runF0 zone nss dom typ cs = runReaderT (evalQueryT (fallbacks (delegation zone nss) dom typ) env (qparam dom typ)) (cs :: [Case])
-- >>> runF  zone nss dom typ cs = fmap (map rdata . filter ((== typ) . rrtype) . answer . fst) <$> runF0 zone nss dom typ cs
-- >>> nssG2 = ("ns1.example.", ["192.0.2.17"]) :| [("ns2.example.", ["192.0.2.33"])]
-- >>> casesG2 = [("192.0.2.17", "a.ts.reasonings.cc.", A, Left RetryLimitExceeded), ("192.0.2.33", "a.ts.reasonings.cc.", A, Right [rd_a "198.51.100.33"])]
-- >>> runF "reasonings.cc." nssG2 "a.ts.reasonings.cc." A casesG2
-- Right [198.51.100.33]
-- >>> runF "reasonings.cc." (("nx.reasonings.cc.", []) :| []) "a.ts.reasonings.cc." A []
-- Left (DnsError ServerFailure [])
-- >>> runF "reasonings.cc." (("nx.reasonings.cc.", []) :| [("adam.ns.cloudflare.com.", [])]) "a.ts.reasonings.cc." A []
-- Right [198.51.100.17]
-- >>> nssG4 = ("ns3.example.", ["192.0.2.18"]) :| [("ns4.example.", ["192.0.2.19"]), ("ns1.example.", ["192.0.2.17"]), ("ns2.example.", ["192.0.2.33"])]
-- >>> casesG4 = [(a, "a.ts.reasonings.cc.", A, Left RetryLimitExceeded) | a <- ["192.0.2.17", "192.0.2.18", "192.0.2.19"]] ++ [("192.0.2.33", "a.ts.reasonings.cc.", A, Right [rd_a "198.51.100.33"])]
-- >>> runF "reasonings.cc." nssG4 "a.ts.reasonings.cc." A casesG4
-- Right [198.51.100.33]
-- >>> nssNS2 = ("ns3.example.", ["192.0.2.18"]) :| [("ns4.example.", ["192.0.2.19"]), ("ns1.example.", ["192.0.2.17"]), ("nx.reasonings.cc.", []), ("adam.ns.cloudflare.com.", [])]
-- >>> casesNS2 = [(a, "a.ts.reasonings.cc.", A, Left RetryLimitExceeded) | a <- ["192.0.2.17", "192.0.2.18", "192.0.2.19"]]
-- >>> runF "reasonings.cc." nssNS2 "a.ts.reasonings.cc." A casesNS2
-- Right [198.51.100.17]
--
delegationFallbacks_
    :: MonadQuery m
    => (String -> m c)
    -> ([(String, [Address])] -> m a)
    -> Int -> Bool -> Int -> Bool -> ([Address] -> m b)
    -> Delegation -> Domain -> TYPE -> m (DNSMessage, Delegation)
delegationFallbacks_ eh fh qparallel disableV6NS dc dnssecOK ah d0@Delegation{..} name typ = do
    paxs  <- dentryToPermAx disableV6NS dentry
    pnss  <- dentryToPermNS zone dentry
    -- Try known IP addresses first
    -- Then try known names if necessary
    fallbacksAx d0 paxs $ fallbacksNS (("<cached>", paxs) :) pnss
  where
    eh' = eh . ("delegationFallbacks: " ++)
    dentry = NE.toList delegationNS
    zone = delegationZone
    eventLog' tag ws = liftIO $ TStat.eventLog $ unwords $ ["iter.ifb", tag, show name, show typ, ":"] ++ ws

    stepAx d axc1@(x:|xs) nexts ea = logging >> norec' `catchQuery` \ex -> nexts (ea . ((ex, axc) :))
      where norec' = (,) <$> norec dnssecOK axc1 name typ <*> pure d
            logging = ah axc >> eventLog' "Ax" (map show axc)
            axc = x:xs
    fallbacksAx d axs fbs = foldr (stepAx d) (\ea -> emsg (ea []) >> fbs) (chunksOf' qparallel axs) id -- qparallel = 2 in delegationFallbacks
      where emsg es = unless (null es) (void $ eh' $ unlines $ unwords [show name, show typ, "failed:"] : map (("  " ++) . show) es)
    resolveNS' ns tyAx = ( resolveNS zone disableV6NS dc ns tyAx <&> \et -> case et of
                             Left (rc, ei)  ->         Left $ show rc ++ " " ++ ei
                             Right x        ->         Right x                     ) `catchQuery`
                           \ex              ->  pure $ Left $ show ex
    stepNS (ns, tyAx) fbs dP aa = do
        res  <- resolveNS' ns tyAx
        dN   <- fillCachedDelegation dP
        let evLog ws = eventLog' "NS" $ [show ns, show tyAx] ++ ws
            fallbacks' as = fbs dN $ aa . ((show ns, as) :)
            left  e    = eh' (unwords [e, "for resolving", show ns, show tyAx]) >> evLog [] >> fallbacks' []
            right axs  = randomized >>= \ps -> evLog (map (show . fst) ps) >> fallbacksAx dN ps (fallbacks' ps)
              where randomized = randomizedPermN [(ip, 53) | (ip, _) <- axs] <&> NE.toList
        either left right res
    zero _d aa = fh (aa []) >> throwDnsError ServerFailure
    randomizedAxs
        | disableV6NS  = pure $ cycle [[A]]
        | otherwise    = icycleM $ randomizedPerm [A, AAAA]
    fallbacksNS aa nss = randomizedAxs >>= \axps -> foldr stepNS zero [(ns, ax) | (ns, axp) <- zip nss axps, ax <- axp] d0 aa
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
resolveNS :: MonadQuery m => Domain -> Bool -> Int -> Domain -> TYPE -> m (Either (RCODE, String) (NonEmpty (IP, RR)))
resolveNS zone disableV6NS dc ns typ = do
    (rc, axs) <- querySection
    list (failEmptyAx rc) (\a as -> pure $ Right $ a :| as) axs
  where
    axPairs = axList disableV6NS (== ns) (,)

    querySection = do
        logLn Log.DEMO $ unwords ["resolveNS:", show (ns, typ), "dc:" ++ show dc, "->", show (succ dc)]
        {- resolve for not sub-level delegation. increase dc (delegation count) -}
        recursiveCD <- maybe NoCheckDisabled (\_ -> CheckDisabled) <$> findNegativeTrustAnchor ns
        {- negative-trust-anchor: <not found>: do DNSSEC checks, <found>: do not DNSSEC checks -}
        localQP (\qp -> qp{requestCD_ = recursiveCD}) $ cacheAnswerAx =<< resolveExactDC (succ dc) ns typ
    cacheAnswerAx (msg, d) = do
        cacheAnswer d ns typ msg $> ()
        pure (rcode msg, withSection rankedAnswer msg $ \rrs _rank -> axPairs rrs)

    failEmptyAx rc = do
        let emptyInfo = "empty " ++ show typ ++ if disableV6NS then " (disable-v6ns)" else ""
        orig <- showQ "orig-query:" <$> asksQP origQuestion_
        let errorInfo = (if rc == NoErr then emptyInfo else show rc) ++ " for NS,"
        pure $ Left (rc, unwords $ errorInfo : ["ns: " ++ show ns ++ ",", "zone: " ++ show zone ++ ",", orig])
{- FOURMOLU_ENABLE -}

---

findNegativeTrustAnchor :: MonadEnv m => Domain -> m (Maybe Domain)
findNegativeTrustAnchor qn = asksEnv negativeTrustAnchors_ <&> \na -> ZMap.lookupApexOn id na qn

-- DNS over UDP53 from a full resolve to an authoritative server
norec :: MonadQuery m => Bool -> NonEmpty Address -> Domain -> TYPE -> m DNSMessage
norec dnssecOK aservers name typ = do
    qcount <- (NE.length aservers +) <$> getQS queryCounter_
    logLn Log.DEBUG ("query count: " ++ show qcount)
    orig <- showQ "orig-query" <$> asksQP origQuestion_
    setQS queryCounter_ qcount
    setQS lastQuery_ (Question name typ IN, NE.toList aservers)
    m <- dispatch qcount orig
    setQS aservMessage_ $ Just m
    pure m
  where
    dispatch qcount orig = do
        maxQueryCount <- asksEnv maxQueryCount_
        let ~exceeded = "max-query-count (==" ++ show maxQueryCount ++ ") exceeded: " ++ showQ' "query" name typ ++ ", " ++ orig
        when (qcount > maxQueryCount) $ logLn Log.WARN exceeded >> left ServerFailure
        -- queryNorec == Norec.norec for DNSQeury == DNS.Do53.Internal.resolve
        queryNorec dnssecOK aservers name typ >>= either left handleResponse
    handleResponse = handleResponseError (NE.toList aservers) throwQuery pure
    left e = cacheDNSError name typ Cache.RankAnswer e >> dnsError e
    dnsError e = throwQuery $ uncurry DnsError $ unwrapDNSErrorInfo e

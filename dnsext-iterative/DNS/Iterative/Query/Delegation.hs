{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module DNS.Iterative.Query.Delegation (
    lookupDelegation,
    delegationWithCache,
    fillCachedDelegation,
    MayDelegation,
    noDelegation,
    hasDelegation,
    mayDelegation,
) where

-- GHC packages
import qualified Data.Set as Set

-- other packages

-- dnsext packages
import qualified DNS.Log as Log
import DNS.RRCache (
    getRank,
    getRanked,
    rankedAdditional,
    rankedAuthority,
 )
import qualified DNS.RRCache as Cache
import DNS.SEC
import qualified DNS.SEC.Verify as SEC
import DNS.Types
import qualified DNS.Types as DNS
import Data.IP (IPv4, IPv6)
import System.Console.ANSI.Types

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Query.Cache
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.Helpers
import DNS.Iterative.Query.Utils
import qualified DNS.Iterative.Query.Verify as Verify
import DNS.Iterative.Query.WitnessInfo (showWitness)

newtype GMayDelegation a
    = MayDelegation (Maybe a)
    deriving (Functor, Foldable, Traversable)

type MayDelegation = GMayDelegation Delegation

noDelegation :: MayDelegation
noDelegation = MayDelegation Nothing

hasDelegation :: Delegation -> MayDelegation
hasDelegation = MayDelegation . Just

mayDelegation :: a -> (Delegation -> a) -> MayDelegation -> a
mayDelegation n h (MayDelegation m) = maybe n h m

{- FOURMOLU_DISABLE -}
-- If Nothing, it is a miss-hit against the cache.
-- If Just NoDelegation, cache hit but no delegation information.
lookupDelegation :: MonadEnv m => Domain -> m (Maybe MayDelegation)
lookupDelegation zone = do
    disableV6NS <- asksEnv disableV6NS_
    let noCachedV4NS es = disableV6NS && all noV4DEntry es

        fromDEs es
            {- all NS records for A are skipped under disableV6NS, so handle as miss-hit NS case -}
            | noCachedV4NS es = Nothing
            --
            {- Nothing case, all NS records are skipped, so handle as miss-hit NS case -}
            | otherwise = list Nothing ((Just .) . hasDelegation') es
          where hasDelegation' de des = hasDelegation $ Delegation zone (de :| des) (NotFilledDS CachedDelegation) [] CachedD

        getDelegation :: MonadEnv m => ([RR], a) -> m (Maybe MayDelegation)
        getDelegation (rrs, _) = do
            {- NS cache hit -}
            let nss = sort $ rrListWith NS (`DNS.rdataField` DNS.ns_domain) zone const rrs
            case nss of
                []     -> return $ Just noDelegation {- hit null NS list, so no delegation -}
                _ : _  -> fromDEs . concat <$> mapM (lookupDEntry zone) nss

    maybe (return Nothing) getDelegation =<< lookupRR zone NS
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
noV4DEntry :: DEntry -> Bool
noV4DEntry (DEonlyNS {})          = True
noV4DEntry (DEwithA4 _ (_:|_))    = False
noV4DEntry (DEwithA6 _ _)         = True
noV4DEntry (DEwithAx _ (_:|_) _)  = False
noV4DEntry (DEstubA4  (_:|_))     = False
noV4DEntry (DEstubA6  (_:|_))     = True
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- Caching while retrieving delegation information from the authoritative server's reply
delegationWithCache :: MonadQuery m => Domain -> [RD_DNSKEY] -> Domain -> DNSMessage -> m MayDelegation
delegationWithCache zone dnskeys dom msg = do
    reqCD <- asksQP requestCD_
    {- There is delegation information only when there is a selectable NS -}
    maybe (notFound $> noDelegation) (found reqCD >>> (<&> hasDelegation)) $ findDelegation nsps adds
  where
    rankedDS = Cache.rkAuthority
    found reqCD k = Verify.cases NoCheckDisabled zone dnskeys (getRanked rankedDS) msg dom DS fromDS (nullDS reqCD k) ncDS (withDS k)
    fromDS = DNS.fromRData . rdata
    nullDS CheckDisabled   k =
        Verify.insecureLog (msgf "no DS, check disabled") $> k []
    nullDS NoCheckDisabled k = do
        {- The witnesses the absence of the DS was proved by, if it was.
           RFC 4035 Sec 5.2 has a resolver conclude that a child zone is
           unsigned only from an authenticated denial of the DS RRset,
           and an empty list is not one: there were no NSEC/NSEC3
           records, or none of them proved anything about this name.
           Going insecure on that would let anybody who can drop records
           from a referral turn off validation for everything below it.

           Only where this zone is signed, which is what a DNSKEY here
           means.  An unsigned parent has no denial to give and is not
           being asked for one. -}
        witnesses <- unsignedDelegationOrNoData
        when (null witnesses && not (null dnskeys)) $
            Verify.bogusError (msgf "no DS, and nothing to say there is none")
        Verify.insecureLog (msgf "no DS, so no verification chain")
        cacheNoData dom DS (getRank rankedDS msg)
        caches $> k []
    ncDS ncLog = ncLog >> Verify.bogusError (msgf "not canonical DS")
    withDS k = Verify.withResult DS msgf $ \dsrds _ _ _ -> caches $> k dsrds
    caches = cacheNS *> cacheAdds

    notFound = Verify.verifyLog Nothing (msgf "no delegation")
    msgf s = "delegation - " ++ s ++ ": " ++ show zone ++ " -> " ++ show dom

    (nsps, cacheNS) = withSection rankedAuthority msg $ \rrs rank ->
        let nsps_ = rrListWith NS (`DNS.rdataField` DNS.ns_domain) dom (,) rrs
        in (nsps_, cacheNoRRSIG (map snd nsps_) rank)

    (adds, cacheAdds) = withSection rankedAdditional msg $ \rrs rank ->
        let axs = filter match rrs in (axs, cacheSection axs rank)
      where
        match rr = rrtype rr `elem` [A, AAAA] && rrname rr `isSubDomainOf` zone && rrname rr `Set.member` nsSet
        nsSet = Set.fromList $ map fst nsps

    unsignedDelegationOrNoData = unsignedDelegationOrNoDataAction zone dnskeys dom A msg
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
fillCachedDelegation :: MonadQuery m => Delegation -> m Delegation
fillCachedDelegation d = list noAvail result . concat =<< mapM fill des
  where
    des = delegationNS d
    fill (DEonlyNS ns) = lookupDEntry (delegationZone d) ns
    fill  e            = pure [e]
    noAvail = logLines Log.DEMO ("fillCachedDelegation - no NS available: " : pprNS des) *> throwDnsError DNS.ServerFailure
    pprNS (e:|es) = map (("  " ++) . show) $ e : es
    result e es = pure $ d{delegationNS = e :| es}
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
lookupDEntry :: MonadEnv m => Domain -> Domain -> m [DEntry]
lookupDEntry zone ns = do
    withERR =<< lookupRR ns Cache.ERR
  where
    withERR Just{}   = pure []  {- skip DEntry with error NS name -}
    withERR Nothing  = do
        let takeV4 = rrListWith A    (`DNS.rdataField` DNS.a_ipv4)    ns const
            takeV6 = rrListWith AAAA (`DNS.rdataField` DNS.aaaa_ipv6) ns const
        lk4 <- fmap (takeV4 . fst) <$> lookupRR ns A
        lk6 <- fmap (takeV6 . fst) <$> lookupRR ns AAAA
        pure $ dentryFromCache zone ns lk4 lk6
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | result value cases of dentryFromCache :
--     []             : miss-hit, skip this NS name, to avoid iterative loop
--     [DEonlyNS {}]  : miss-hit
--     [DEwithA...]   : hit
--
-- >>> :seti -XOverloadedStrings
-- >>> dentryFromCache "example." "ns.example." Nothing Nothing
-- []
-- >>> dentryFromCache "example." "ns.example." Nothing (Just [])
-- []
-- >>> dentryFromCache "example." "ns.example." (Just []) Nothing
-- []
-- >>> dentryFromCache "example." "ns.example." (Just []) (Just [])
-- []
-- >>> dentryFromCache "a.example." "ns.example." Nothing Nothing
-- [DEonlyNS "ns.example."]
-- >>> dentryFromCache "a.example." "ns.example." Nothing (Just [])
-- [DEonlyNS "ns.example."]
-- >>> dentryFromCache "a.example." "ns.example." (Just []) Nothing
-- [DEonlyNS "ns.example."]
-- >>> dentryFromCache "a.example." "ns.example." (Just []) (Just [])
-- []
-- >>> dentryFromCache "a.example." "ns.example." (Just ["192.0.2.1"]) (Just [])
-- [DEwithA4 "ns.example." (192.0.2.1 :| [])]
-- >>> dentryFromCache "a.example." "ns.example." (Just []) (Just ["2001:db8::1"])
-- [DEwithA6 "ns.example." (2001:db8::1 :| [])]
-- >>> dentryFromCache "a.example." "ns.example." (Just ["192.0.2.1"]) (Just ["2001:db8::1"])
-- [DEwithAx "ns.example." (192.0.2.1 :| []) (2001:db8::1 :| [])]
dentryFromCache :: Domain -> Domain -> Maybe [IPv4] -> Maybe [IPv6] -> [DEntry]
dentryFromCache zone ns = dispatch
  where
    missHit
        | ns `DNS.isSubDomainOf` zone  = []  {- miss-hit with sub-domain case cause iterative loop. null result to skip this NS -}
        | otherwise                    = [DEonlyNS ns]
    dispatch Nothing       Nothing          = missHit  {- A: miss-hit     AAAA: miss-hit                       -}
    dispatch Nothing       (Just [])        = missHit  {- A: miss-hit     AAAA: hit NoData  , assumes miss-hit -}
    dispatch (Just [])     Nothing          = missHit  {- A: hit NoData   AAAA: miss-hit    , assumes miss-hit -}
    dispatch Nothing       (Just (i:is))    = [DEwithA6 ns (i :| is)]
    dispatch (Just (i:is)) Nothing          = [DEwithA4 ns (i :| is)]
    dispatch (Just i4s)   (Just i6s)        = foldIPList'
                                              []       {- A: hit NoData   AAAA: hit NoData  , maybe wrong cache, skip this NS -}
                                              (\v4    -> [DEwithA4 ns v4])
                                              (\v6    -> [DEwithA6 ns v6])
                                              (\v4 v6 -> [DEwithAx ns v4 v6])
                                              i4s i6s
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
unsignedDelegationOrNoDataAction
    :: MonadQuery m
    => Domain -> [RD_DNSKEY]
    -> Domain -> TYPE -> DNSMessage
    -> m [RRset]
unsignedDelegationOrNoDataAction zone dnskeys qname_ qtype_ msg = nsec
  where
    nsec  = Verify.nsecWithValid   dnskeys rankedAuthority msg nullNSEC invalidK nsecK
    nullNSEC = nsec3
    nsecK  ranges rrsets doCache =
        Verify.runHandlers "cannot handle NSEC UnsignedDelegation/NoDatas:"  noWitnessK $
        handle unsignedDelegation resultK .
        handle wildcardNoData     resultK .
        handle noData             resultK
      where
        handle = Verify.mkHandler ranges rrsets doCache
        unsignedDelegation rs  = SEC.unsignedDelegationNSEC   zone rs qname_
        wildcardNoData     rs  = SEC.wildcardNoDataNSEC       zone rs qname_ qtype_
        noData             rs  = SEC.noDataNSEC               zone rs qname_ qtype_

    nsec3 = Verify.nsec3WithValid  dnskeys rankedAuthority msg nullK    invalidK nsec3K
    nsec3K ranges rrsets doCache =
        Verify.runHandlers "cannot handle NSEC3 UnsignedDelegation/NoDatas:" noWitnessK $
        handle unsignedDelegation resultK .
        handle wildcardNoData     resultK .
        handle noData             resultK
      where
        handle = Verify.mkHandler ranges rrsets doCache
        unsignedDelegation rs  = SEC.unsignedDelegationNSEC3  zone rs qname_
        wildcardNoData     rs  = SEC.wildcardNoDataNSEC3      zone rs qname_ qtype_
        noData             rs  = SEC.noDataNSEC3              zone rs qname_ qtype_

    nullK = noverify "no NSEC/NSEC3 records" $> []
    invalidK s = failed $ "invalid NSEC/NSEC3: " ++ traceInfo ++ " : " ++ s
    noWitnessK s =noverify ("nsec witness not found: " ++ traceInfo ++ " : " ++ s) $> []
    resultK  w rrsets _ = success w *> winfo (showWitness w) $> rrsets

    success w = putLog (Just Green) $ "nsec verification success - " ++ witnessInfo w
    winfo wi = putLog (Just Cyan) $ unlines $ map ("  " ++) wi
    noverify s = putLog (Just Yellow) $ "nsec no verification - " ++ s
    failed s = putLog (Just Red) ( "nsec verification failed - " ++ s) *> throwDnsError DNS.ServerFailure

    putLog color s = clogLn Log.DEMO color s

    witnessInfo w = SEC.witnessName w ++ ": " ++ witnessTypeInfo w
    witnessTypeInfo w = case SEC.witnessType w of
        SEC.NwUnsignedDelegation  -> traceInfo
        _                         -> qinfo
    traceInfo = show zone ++ " -> " ++ show qname_
    qinfo = show qname_ ++ " " ++ show qtype_
{- FOURMOLU_ENABLE -}

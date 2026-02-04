{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Query.Env (
    Env (..),
    newEmptyEnv,
    --
    newReloadInfo,
    --
    cropMaxNegativeTTL,
    cropFailureRcodeTTL,
    --
    setRRCacheOps,
    setTimeCache,
    --
    readRootHint,
    setRootHint,
    --
    TrustAnchors,
    readTrustAnchors,
    setRootAnchor,
    --
    getChaosZones,
    getLocalZones,
    getStubZones,
    getNegTrustAnchors,
    --
    LocalZoneType (..),
    RR,
    --
    identityRefuse,
    identityHash,
    identityHost,
    identityString,
    --
    versionRefuse,
    versionBlank,
    versionShow,
    versionString,
    --
    getUpdateHistogram,
) where

-- GHC packages
import Control.Concurrent (getNumCapabilities)
import Data.Array (Ix, listArray, (!))
import qualified Data.ByteString.Char8 as C8
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.TLS (defaultKeyLogger)
import System.Posix (getSystemID, nodeName)
import System.Timeout (timeout)

-- other packages
import Crypto.Hash (hashWith)
import qualified Crypto.Hash.Algorithms as HA
import qualified Data.ByteArray as BA
import Data.ByteString.Base16 as B16

-- dnsext packages
import DNS.Do53.Internal (newConcurrentGenId)
import DNS.RRCache (RRCacheOps (..))
import qualified DNS.RRCache as Cache
import DNS.SEC
import DNS.TimeCache (TimeCache (..), getTime, noneTimeCache)
import DNS.Types
import DNS.Types.Time (getCurrentTimeUsec)
import DNS.ZoneFile (Record (R_RR))
import qualified DNS.ZoneFile as Zone

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.DefaultLocal (defaultLocal)
import qualified DNS.Iterative.Query.DefaultLocal as Local
import DNS.Iterative.Query.Helpers
import qualified DNS.Iterative.Query.LocalZone as Local
import qualified DNS.Iterative.Query.StubZone as Stub
import qualified DNS.Iterative.Query.Verify as Verify
import qualified DNS.Iterative.Query.ZoneMap as ZMap
import DNS.Iterative.RootServers (getRootServers)
import DNS.Iterative.RootTrustAnchors (rootSepDSs)
import DNS.Iterative.Stats

version :: String
version = "0.0.0.20251105"

{- FOURMOLU_DISABLE -}
-- | Creating a new 'Env'.
newEmptyEnv :: IO Env
newEmptyEnv = do
    genId    <- newConcurrentGenId
    rootRef  <- newIORef Nothing
    statsInfo <- getNumCapabilities <&> \cap -> [("threads", show cap), ("version", version)]
    stats <- newStats
    let TimeCache {..} = noneTimeCache
    pure $
        Env
        { shortLog_ = False
        , logLines_ = \_ _ ~_ -> pure ()
        , logDNSTAP_ = \_ -> pure ()
        , disableV6NS_ = False
        , rootAnchor_ = FilledDS rootSepDSs
        , rootHint_ = rootHint
        , chaosZones_ = mempty
        , localZones_ = mempty
        , stubZones_ = mempty
        , negativeTrustAnchors_ = mempty
        , maxNegativeTTL_ = 3600
        , failureRcodeTTL_ = 180
        , maxQueryCount_ = 64
        , udpLimit_ = 1200
        , insert_ = \_ _ _ _ -> pure ()
        , getCache_ = pure $ Cache.empty 0
        , expireCache_ = \_ -> pure ()
        , removeCache_ = \_ -> pure()
        , filterCache_ = \_ -> pure()
        , clearCache_ = pure ()
        , currentRoot_ = rootRef
        , putSSLKeyLog_ = defaultKeyLogger
        , currentSeconds_ = getTime
        , currentTimeUsec_ = getCurrentTimeUsec
        , timeString_ = getTimeStr
        , idGen_ = genId
        , reloadInfo_ = []
        , statsInfo_ = statsInfo
        , stats_ = stats
        , nsid_ = Nothing
        , updateHistogram_ = \_ _ -> pure ()
        , timeout_ = timeout 5000000
        }
{- FOURMOLU_ENABLE -}

---

newtype ReloadIx = ReloadIx Int deriving (Eq, Ord, Enum, Ix)

{- FOURMOLU_DISABLE -}
pattern ReloadIxMin :: ReloadIx
pattern ReloadIxMin  = ReloadIx 0

pattern NoKeepCount  :: ReloadIx
pattern NoKeepCount   = ReloadIx 0
pattern NoKeepFailed :: ReloadIx
pattern NoKeepFailed  = ReloadIx 1
pattern NoKeepLast   :: ReloadIx
pattern NoKeepLast    = ReloadIx 2

pattern KeepCount    :: ReloadIx
pattern KeepCount     = ReloadIx 3
pattern KeepFailed   :: ReloadIx
pattern KeepFailed    = ReloadIx 4
pattern KeepLast     :: ReloadIx
pattern KeepLast      = ReloadIx 5

pattern ReloadIxMax :: ReloadIx
pattern ReloadIxMax  = ReloadIx 5
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
newReloadInfo :: Num a => IO a -> IO ([(String, IO a)], IO (), IO (), IO (), IO ())
newReloadInfo getTime = do
    let l = ReloadIxMin
        u = ReloadIxMax
    ria <- listArray (l, u) <$> replicateM (fromEnum u - fromEnum l + 1) (newIORef 0)

    let count ix    = atomicModifyIORef' (ria ! ix) (\c -> (c + 1, ()))
        settime ix  = getTime >>= \t -> atomicModifyIORef' (ria ! ix) (\_ -> (t, ()))
        noKeepSuccess  = count NoKeepCount >> settime NoKeepLast
        noKeepFailure  = count NoKeepFailed
        keepSuccess    = count KeepCount   >> settime KeepLast
        keepFailure    = count KeepFailed

    let getc ix = readIORef (ria ! ix)
        reloadInfo =
            [ ("reload_count"      , getc NoKeepCount   )
            , ("reload_failed"     , getc NoKeepFailed  )
            , ("reload_last"       , getc NoKeepLast    )
            , ("keepcache_count"   , getc KeepCount     )
            , ("keepcache_failed"  , getc KeepFailed    )
            , ("keepcache_last"    , getc KeepLast      )
            ]

    return (reloadInfo, noKeepSuccess, noKeepFailure, keepSuccess, keepFailure)
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
cropMaxNegativeTTL :: Integral a => a -> TTL
cropMaxNegativeTTL nttl
    | nttl > 21600  = 21600
    | nttl <    30  =    30
    | otherwise     = fromIntegral nttl
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
cropFailureRcodeTTL :: Integral a => a -> TTL
cropFailureRcodeTTL fttl
    {- RFC 9520 - 3.2 Caching
       https://datatracker.ietf.org/doc/html/rfc9520#name-caching
       "Consistent with [RFC2308], resolution failures MUST NOT be cached for longer than 5 minutes."  -}
    | fttl > 300  = 300
    {- RFC 8767 - 5. Example Method
       https://datatracker.ietf.org/doc/html/rfc8767#name-example-method
       "Attempts to refresh from non-responsive or otherwise failing authoritative nameservers
        are recommended to be done no more frequently than every 30 seconds."                          -}
    | fttl <  30  =  30
    | otherwise   = fromIntegral fttl
{- FOURMOLU_ENABLE -}

setRRCacheOps :: RRCacheOps -> Env -> Env
setRRCacheOps RRCacheOps{..} env0 =
    env0
        { insert_ = insertCache
        , getCache_ = readCache
        , expireCache_ = expireCache
        , removeCache_ = removeCache
        , filterCache_ = filterCache
        , clearCache_ = clearCache
        }

setTimeCache :: TimeCache -> Env -> Env
setTimeCache TimeCache{..} env0 =
    env0
        { currentSeconds_ = getTime
        , timeString_ = getTimeStr
        }

---

readRootHint :: FilePath -> IO Delegation
readRootHint = withRootDelegation fail pure <=< getRootServers

setRootHint :: Maybe Delegation -> Env -> Env
setRootHint md env0 = maybe env0 (\d -> env0{rootHint_ = d}) md

---

type TrustAnchors = Map Domain MayFilledDS

{- FOURMOLU_DISABLE -}
readTrustAnchors :: [FilePath] -> IO TrustAnchors
readTrustAnchors ps = do
    pairs <- mapM readAnchor ps
    let (ds, ks) = unzip pairs
        dss  = ngroup $ concat ds
        keys = ngroup $ concat ks
        results = merge fst fst nullKEY nullDS cons dss keys
        lefts = ["  skipping, mismatch between DS and SEP, zone = " <> show n : map ("    " ++) e | (n, Left e) <- results]
    unless (null lefts) $ do
        putStrLn "trust-anchor: mismatch zone(s) are found"
        mapM_ (putStr . unlines) lefts
    pure $ Map.fromList [(n, r) | (n, Right r) <- results]
  where
    ngroup :: [(Domain, a)] -> [(Domain, [a])]
    ngroup = map repn . NE.groupBy ((==) `on` fst) . sortOn fst
    repn (x :| xs) = (fst x, map snd (x : xs))
    --
    nullKEY (n,d) = ((n, pure $ FilledDS d) :)
    nullDS  (n,k) xs = list xs (\s ss -> (n, pure $ AnchorSEP [] $ s:|ss) : xs) k
    cons (n,d) (_,k) xs = either mismatch match $ Verify.sepDNSKEY d n k
      where
        mismatch e = (n, Left $ e : map show d ++ map show k) : xs
        match vs = case unzipNE vs of (sep, ds:|dss) -> (n, pure $ AnchorSEP (ds:dss) sep) : xs
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
readAnchor :: FilePath -> IO ([(Domain, RD_DS)], [(Domain, RD_DNSKEY)])
readAnchor path = do
    rs <- Zone.parseFile path
    let rrs = [ rr | R_RR rr <- rs ]
        dss  = [ (rrname, ds) | ResourceRecord{ rrtype = DS    , .. } <- rrs, Just ds <- [fromRData rdata] ]
        keys = [ (rrname, ky) | ResourceRecord{ rrtype = DNSKEY, .. } <- rrs, Just ky <- [fromRData rdata] ]
    pure (dss, keys)
{- FOURMOLU_ENABLE -}

setRootAnchor :: TrustAnchors -> Env -> Env
setRootAnchor as env0 = maybe env0 (\v -> env0{rootAnchor_ = v}) $ Map.lookup (fromString ".") as

---

getChaosZones :: [(Domain, LocalZoneType, [RR])] -> LocalZones
getChaosZones = localZones

getLocalZones :: [(Domain, LocalZoneType, [RR])] -> LocalZones
getLocalZones lzones0 = localZones $ Local.unionZones defaultLocal lzones0

localZones :: [(Domain, LocalZoneType, [RR])] -> LocalZones
localZones lzones | localName <- Local.nameMap lzones = (Local.apexMap localName lzones, localName)

{- FOURMOLU_DISABLE -}
identityRefuse  :: IO [(Domain, LocalZoneType, [RR])]
identityRefuse  = pure Local.hideIdentity

identityHash    :: IO [(Domain, LocalZoneType, [RR])]
identityHash    = Local.identity . hash' <$> getNode
  where hash' n = take 7 $ C8.unpack $ B16.encode $ BA.convert $ hashWith HA.SHA256 (fromString n :: ByteString)

identityHost    :: IO [(Domain, LocalZoneType, [RR])]
identityHost    = Local.identity <$> getNode

identityString  :: String -> IO [(Domain, LocalZoneType, [RR])]
identityString  = pure . Local.identity

getNode :: IO String
getNode = nodeName <$> getSystemID
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
versionRefuse  :: [(Domain, LocalZoneType, [RR])]
versionRefuse  = Local.hideVersion

versionBlank   :: [(Domain, LocalZoneType, [RR])]
versionBlank   = Local.version   "bowline"

versionShow    :: [(Domain, LocalZoneType, [RR])]
versionShow    = Local.version $ "bowline " ++ version

versionString  :: String -> [(Domain, LocalZoneType, [RR])]
versionString  = Local.version
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
getStubZones :: [(Domain, [Domain], [Address])] -> TrustAnchors -> IO StubZones
getStubZones zones anchors = either fail pure $ Stub.getStubMap zones'
  where
    zones' = [ (apex, ns, as, dstate) | (apex, ns, as) <- zones, let dstate = fromMaybe (FilledDS []) $ Map.lookup apex anchors ]
{- FOURMOLU_ENABLE -}

---

getNegTrustAnchors :: [Domain] -> NegTrustAnchors
getNegTrustAnchors xs = Map.fromList $ ZMap.subdomainSemilatticeOn id xs

module DNS.Iterative.Query.Class (
    MonadEnv (..),
    MonadContext (..),
    MonadQuery (..),
    setQS,
    getQS,
    throwDnsError,
    --
    Env (..),
    LocalZoneType (..),
    LocalZones,
    StubZones,
    NegTrustAnchors,
    MayVerifiedRRS (..),
    mayVerifiedRRS,
    CasesNotValid (..),
    notValidNoSig,
    notValidCheckDisabled,
    notValidInvalid,
    RRset (..),
    CasesNotFilledDS (..),
    MayFilledDS (..),
    DFreshState (..),
    Address,
    Delegation (..),
    DEntry (..),
    delegationEntry,
    --
    QueryParam (..),
    queryParamH,
    RequestDO (..),
    RequestCD (..),
    RequestAD (..),
    --
    QueryState (..),
    newQueryState,
    --
    ExtraError (..),
    extraError,
    QueryError (..),
    --
    RR,
    Result,
    ResultRRS',
    ResultRRS,
) where

-- GHC packages
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IP (IP, IPv4, IPv6)
import Data.Map.Strict (Map)
import Network.Socket (PortNumber)

-- dnsext-types
import DNS.Types hiding (flags)
import qualified DNS.Types as DNS

-- dnsext-dnssec
import DNS.SEC (RD_DNSKEY, RD_DS, RD_RRSIG)

-- dnsext-utils
import qualified DNS.Log as Log
import DNS.RRCache (Cache, Ranking)
import qualified DNS.RRCache as Cache
import qualified DNS.TAP.Schema as DNSTAP
import DNS.Transport.Types (Synthesis)
import DNS.WorkerStats (WorkerStatOP)

-- dnsext-do53
import DNS.Do53.Client (Reply)

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Stats (Stats)

----------
-- tagless-final effect interfaces

class MonadIO m => MonadEnv m where
    asksEnv :: (Env -> a) -> m a
    asksWS :: (WorkerStatOP -> a) -> m a

class MonadEnv m => MonadContext m where
    asksQP :: (QueryParam -> a) -> m a
    localQP :: (QueryParam -> QueryParam) -> m a -> m a
    asksQS :: (QueryState -> a) -> m a
    throwQuery :: QueryError -> m a
    catchQuery :: m a -> (QueryError -> m a) -> m a

class MonadContext m => MonadQuery m where
    queryNorec :: Bool -> NonEmpty Address -> Domain -> TYPE -> m (Either DNSError DNSMessage)

setQS :: MonadContext m => (QueryState -> StateVal a n) -> a -> m ()
setQS f x = do
    StateVal ref <- asksQS f
    liftIO $ atomicModifyIORef' ref (\_ -> (x, ()))

getQS :: MonadContext m => (QueryState -> StateVal a n) -> m a
getQS f = do
    StateVal ref <- asksQS f
    liftIO $ readIORef ref

throwDnsError :: MonadContext m => DNSError -> m a
throwDnsError = throwQuery . (`DnsError` [])

----------
-- Env context type for Monad

data Env = Env
    { shortLog_ :: Bool
    , logLines_ :: Log.PutLines IO
    , logDNSTAP_ :: DNSTAP.Message -> IO ()
    , disableV6NS_ :: Bool
    , rootAnchor_ :: MayFilledDS
    , rootHint_ :: Delegation
    , chaosZones_ :: LocalZones
    , localZones_ :: LocalZones
    , localSynthZones_ :: Map Synthesis LocalZones
    , stubZones_ :: StubZones
    , negativeTrustAnchors_ :: NegTrustAnchors
    , maxNegativeTTL_ :: TTL
    , maxCacheTTL_ :: TTL
    , failureRcodeTTL_ :: TTL
    , maxQueryCount_ :: Int
    , udpLimit_ :: Word16
    , insert_ :: Question -> TTL -> Cache.Hit -> Ranking -> IO ()
    , getCache_ :: IO Cache
    , expireCache_ :: EpochTime -> IO ()
    , removeCache_ :: Question -> IO ()
    , filterCache_ :: (Question -> EpochTime -> Cache.Hit -> Ranking -> Bool) -> IO ()
    , clearCache_ :: IO ()
    , currentRoot_ :: IORef (Maybe Delegation)
    , putSSLKeyLog_ :: String -> IO ()
    , currentSeconds_ :: IO EpochTime
    , currentTimeUsec_ :: IO EpochTimeUsec
    , timeString_ :: IO ShowS
    , idGen_ :: IO DNS.Identifier
    , reloadInfo_ :: [(String, IO Int64)]
    , statsInfo_ :: [(String, String)]
    , stats_ :: Stats
    , nsid_ :: Maybe OD_NSID
    , updateHistogram_ :: Integer -> Stats -> IO ()
    , timeout_ :: IO Reply -> IO (Maybe Reply)
    }

----------
-- Local Zone

{- FOURMOLU_DISABLE -}
data LocalZoneType
    = LZ_Deny
    | LZ_Refuse
    | LZ_Static
    {- LZ_Transparent -}
    {- LZ_TypeTransparent -}
    | LZ_Redirect
    deriving Show
{- FOURMOLU_ENABLE -}

type LocalZones = (Map Domain [(Domain, LocalZoneType, [RRset])], Map Domain [RRset])
type StubZones = Map Domain [Delegation]
type NegTrustAnchors = Map Domain [Domain]

----------
-- DNSSEC Verification state and RRset

{- FOURMOLU_DISABLE -}
data MayVerifiedRRS
    = NotValidRRS CasesNotValid  {- not verified or invalid -}
    | ValidRRS [RD_RRSIG]        {- any RRSIG is passed. [RD_RRSIG] should be not null -}
    deriving Eq

mayVerifiedRRS :: a -> a -> (String -> a) -> ([RD_RRSIG] -> a) -> MayVerifiedRRS -> a
mayVerifiedRRS noSig checkDisabled invalid valid m = case m of
    NotValidRRS  NV_NoSig           ->  noSig
    NotValidRRS  NV_CheckDisabled   ->  checkDisabled
    NotValidRRS (NV_Invalid es)     ->  invalid es
    ValidRRS sigs                   ->  valid sigs

data CasesNotValid
    = NV_NoSig                   {- request RRSIG, but not returned -}
    | NV_CheckDisabled           {- only for check-disabled state, so unknown whether verifiable or not. may verify again -}
    | NV_Invalid String          {- RRSIG exists, but no good RRSIG is found -}
    deriving (Eq, Show)

notValidNoSig :: MayVerifiedRRS
notValidNoSig = NotValidRRS NV_NoSig

notValidCheckDisabled :: MayVerifiedRRS
notValidCheckDisabled = NotValidRRS NV_CheckDisabled

notValidInvalid :: String -> MayVerifiedRRS
notValidInvalid = NotValidRRS . NV_Invalid

instance Show MayVerifiedRRS where
  show = mayVerifiedRRS "NotValidRRS NoSig" "NotValidRRS CheckDisabled" ("NotValidRRS_Invalid " ++) (("ValidRRS " ++) . show)
{- FOURMOLU_ENABLE -}

data RRset = RRset
    { rrsName :: Domain
    , rrsType :: TYPE
    , rrsClass :: CLASS
    , rrsTTL :: TTL
    , rrsRDatas :: [RData]
    , rrsMayVerified :: MayVerifiedRRS
    }
    deriving (Show)

----------
-- Delegation

{- FOURMOLU_DISABLE -}
-- | The cases that generate `NotFilled`
data CasesNotFilledDS
    = CachedDelegation  {- intermediate state of reconstructing delegation-info using cache -}
    | ServsChildZone    {- when child(sub-domain) zone shares same authoritative server,
                            delegation-info that should contain DS records is not returned -}
    deriving Show

data MayFilledDS
    = NotFilledDS CasesNotFilledDS
    | FilledDS [RD_DS]                        {- Filled [] - DS does not exist | Filled (_:_) - DS exist, include DS only anchor -}
    | AnchorSEP [RD_DS] (NonEmpty RD_DNSKEY)  {- with specified trust-anchor dnskey -}
    deriving (Show)

data DFreshState
    = FreshD   {- Got from authoritative server directly -}
    | CachedD  {- From cache -}
    deriving Show
{- FOURMOLU_ENABLE -}

type Address = (IP, PortNumber)

-- | Delegation information for domain
data Delegation = Delegation
    { delegationZone :: Domain
    -- ^ Destination zone domain
    , delegationNS :: NonEmpty DEntry
    -- ^ NS infos of destination zone, get from source zone NS
    , delegationDS :: MayFilledDS
    -- ^ SEP DNSKEY signature of destination zone, get from source zone NS
    , delegationDNSKEY :: [RD_DNSKEY]
    -- ^ Destination DNSKEY set, get from destination NS
    , delegationFresh :: DFreshState
    -- ^ Fresh or Cached
    }
    deriving (Show)

data DEntry
    = DEwithAx !Domain !(NonEmpty IPv4) !(NonEmpty IPv6)
    | DEwithA4 !Domain !(NonEmpty IPv4)
    | DEwithA6 !Domain !(NonEmpty IPv6)
    | DEonlyNS !Domain
    | DEstubA4 !(NonEmpty (IPv4, PortNumber))
    | DEstubA6 !(NonEmpty (IPv6, PortNumber))
    deriving (Show)

{- FOURMOLU_DISABLE -}
delegationEntry
    :: (Domain -> NonEmpty IPv4 -> NonEmpty IPv6 -> a)
    -> (Domain -> NonEmpty IPv4 -> a) -> (Domain -> NonEmpty IPv6 -> a) -> (Domain -> a)
    -> (NonEmpty (IPv4, PortNumber) -> a) -> (NonEmpty (IPv6, PortNumber) -> a)
    -> DEntry -> a
delegationEntry ax a4 a6 ns s4 s6 de = case de of
    DEwithAx d n4 n6  -> ax d n4 n6
    DEwithA4 d n4     -> a4 d n4
    DEwithA6 d n6     -> a6 d n6
    DEonlyNS d        -> ns d
    DEstubA4 na4      -> s4 na4
    DEstubA6 na6      -> s6 na6
{- FOURMOLU_ENABLE -}

----------
-- QueryParam context type for Monad

data QueryParam = QueryParam
    { origQuestion_ :: Question
    , requestDO_ :: RequestDO
    , requestCD_ :: RequestCD
    , requestAD_ :: RequestAD
    }

queryParamH :: Question -> DNSFlags -> EDNSheader -> QueryParam
queryParamH q flags eh = QueryParam q (toRequestDO eh) (toRequestCD flags) (toRequestAD flags)

{- Datatypes for request flags to pass iterative query.
  * DO (DNSSEC OK) must be 1 for DNSSEC available resolver
    * https://datatracker.ietf.org/doc/html/rfc4035#section-3.2.1 The DO Bit
  * CD (Checking Disabled)
  * AD (Authenticated Data)
    * https://datatracker.ietf.org/doc/html/rfc6840#section-5.7 Setting the AD Bit on Queries
      "setting the AD bit in a query as a signal indicating that the requester understands
       and is interested in the value of the AD bit in the response" -}
data RequestDO
    = DnssecOK
    | NoDnssecOK
    deriving (Eq, Show)

data RequestCD
    = CheckDisabled
    | NoCheckDisabled
    deriving (Eq, Show)

data RequestAD
    = AuthenticatedData
    | NoAuthenticatedData
    deriving (Eq, Show)

toRequestDO :: EDNSheader -> RequestDO
toRequestDO = ednsHeaderCases (bool NoDnssecOK DnssecOK . ednsDnssecOk) NoDnssecOK NoDnssecOK

toRequestCD :: DNSFlags -> RequestCD
toRequestCD = bool NoCheckDisabled CheckDisabled . chkDisable

toRequestAD :: DNSFlags -> RequestAD
toRequestAD = bool NoAuthenticatedData AuthenticatedData . authenData

----------
-- QueryState context type for Monad

data QueryCount
data LastQuery
data AservMessage
newtype StateVal a n = StateVal (IORef a)

{- FOURMOLU_DISABLE -}
data QueryState = QueryState
    { queryCounter_  :: StateVal Int QueryCount
    , lastQuery_     :: StateVal (Question, [Address]) LastQuery
    , aservMessage_  :: StateVal (Maybe DNSMessage) AservMessage
    }
{- FOURMOLU_ENABLE -}

newStateVal :: a -> IO (StateVal a n)
newStateVal iv = StateVal <$> newIORef iv

newQueryState :: IO QueryState
newQueryState = do
    refq <- newStateVal 0
    rlsq <- newStateVal (Question (fromString "") A IN, [])
    rasm <- newStateVal Nothing
    pure $ QueryState refq rlsq rasm

----------
-- QueryError

data ExtraError
    = ErrorNotResp
    | ErrorEDNS DNS.EDNSheader
    | ErrorRCODE DNS.RCODE
    | ErrorBogus String
    deriving (Show)

{- FOURMOLU_DISABLE -}
extraError :: a -> (EDNSheader -> a) -> (RCODE -> a) -> (String -> a) -> ExtraError -> a
extraError notResp errEDNS errRCODE bogus fe = case fe of
    ErrorNotResp  -> notResp
    ErrorEDNS e   -> errEDNS e
    ErrorRCODE e  -> errRCODE e
    ErrorBogus s  -> bogus s
{- FOURMOLU_ENABLE -}

data QueryError
    = DnsError DNSError [String]
    | ExtraError ExtraError [Address] (Maybe DNSMessage)
    deriving (Show)

----------
-- synonyms

type RR = ResourceRecord

{- response code, answer section, authority section -}
type Result = (RCODE, DNSFlags, [RR], [RR])
type ResultRRS' a = (a, [RRset], [RRset])
type ResultRRS = ResultRRS' RCODE

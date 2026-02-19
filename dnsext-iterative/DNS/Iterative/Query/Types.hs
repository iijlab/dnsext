{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DNS.Iterative.Query.Types (
    VResult (..),
    queryParam,
    queryParamIN,
    queryControls',
    ContextT,
    chainedStateDS,
    QueryT,
    runQueryT,
    evalQueryT,
    DNSQuery,
    runDNSQuery,
    handleResponseError,
) where

-- GHC packages

-- other packages

-- dnsext packages
import DNS.Do53.Client (QueryControls)
import DNS.Do53.Internal (queryControls)
import DNS.Types
import qualified DNS.Types as DNS

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.Norec (norec)

----------

queryParam :: Question -> QueryControls -> QueryParam
queryParam q = queryControls' $ queryParamH q

queryParamIN :: Domain -> TYPE -> QueryControls -> QueryParam
queryParamIN dom typ = queryParam (Question dom typ IN)

queryControls' :: (DNSFlags -> EDNSheader -> a) -> QueryControls -> a
queryControls' h = queryControls (\mf eh _ -> h (mf defaultQueryDNSFlags) eh)

----------

type ContextT m = ReaderT Env (ReaderT QueryParam (ReaderT QueryState m))
type QueryT m = ExceptT QueryError (ContextT m)
type DNSQuery = QueryT IO

instance MonadIO m => MonadEnv (QueryT m) where
    asksEnv = lift . asks
    {-# INLINEABLE asksEnv #-}

instance MonadIO m => MonadContext (QueryT m) where
    asksQP = lift . lift . asks
    {-# INLINEABLE asksQP #-}
    localQP f = mapExceptT $ mapReaderT $ local f
    {-# INLINEABLE localQP #-}
    asksQS = lift . lift . lift . asks
    {-# INLINEABLE asksQS #-}
    throwQuery = throwE
    {-# INLINEABLE throwQuery #-}
    catchQuery = catchE
    {-# INLINEABLE catchQuery #-}

instance MonadQuery DNSQuery where
    queryNorec dnssecOK aservers name typ = asksEnv id >>= \env -> norec env dnssecOK aservers name typ
    {-# INLINEABLE queryNorec #-}

runQueryT :: MonadIO m => QueryT m a -> Env -> QueryParam -> m (Either QueryError a, QueryState)
runQueryT q e p = do
    s <- liftIO newQueryState
    (,) <$> runReaderT (runReaderT (runReaderT (runExceptT q) e) p) s <*> pure s

evalQueryT :: MonadIO m => QueryT m a -> Env -> QueryParam -> m (Either QueryError a)
evalQueryT q e p = fst <$> runQueryT q e p

runDNSQuery :: DNSQuery a -> Env -> QueryParam -> IO (Either QueryError a)
runDNSQuery = evalQueryT

{- FOURMOLU_DISABLE -}
-- example instances
-- - responseErrEither = handleResponseError Left Right  :: DNSMessage -> Either QueryError DNSMessage
-- - responseErrDNSQuery = handleResponseError throwError pure  :: DNSMessage -> DNSQuery DNSMessage
handleResponseError :: [Address] -> (QueryError -> p) -> (DNSMessage -> p) -> DNSMessage -> p
handleResponseError addrs e f msg = exerror $ \ee -> e $ ExtraError ee addrs $ Just msg
  where
    exerror eh
        | not (DNS.isResponse $ DNS.flags msg)               = eh   ErrorNotResp
        | InvalidEDNS <- DNS.ednsHeader msg                  = eh $ ErrorEDNS $ DNS.ednsHeader msg
        | DNS.rcode msg `notElem` [DNS.NoErr, DNS.NXDomain]  = eh $ ErrorRCODE $ DNS.rcode msg
        | otherwise                                          = f msg
{- FOURMOLU_ENABLE -}

----------
-- Delegation

{- FOURMOLU_DISABLE -}
chainedStateDS :: Delegation -> Bool
chainedStateDS d = case delegationDS d of
    NotFilledDS _     -> False
    FilledDS []       -> False
    FilledDS (_ : _)  -> True
    AnchorSEP {}      -> True
{- FOURMOLU_ENABLE -}

----------
-- results

-- result tag from DNSSEC verification
data VResult
    = VR_Secure
    | VR_Insecure
    | VR_Bogus
    deriving (Show)

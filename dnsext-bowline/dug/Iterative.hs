{-# LANGUAGE RecordWildCards #-}

module Iterative (iterativeQuery, getRootV6) where

--
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import System.Timeout (timeout)

--

import DNS.Do53.Client (QueryControls)
import DNS.Iterative.Internal (Delegation (..), delegationEntry)
import DNS.Iterative.Query (
    Env (..),
    newEmptyEnv,
    readRootHint,
    readTrustAnchors,
    resolveResponseIterative,
    setRRCacheOps,
    setRootAnchor,
    setRootHint,
    setTimeCache,
 )
import qualified DNS.Log as Log
import qualified DNS.RRCache as Cache
import DNS.TimeCache (getTime, newTimeCache)
import DNS.Transport.Types (Synthesis)
import Data.IP (IP (IPv6))
import Network.Socket (PortNumber)

import DNS.Types

import Types (Options (..), shortLog)

{- FOURMOLU_DISABLE -}
getRootV6 :: IO [(IP, PortNumber)]
getRootV6 = do
    Env{rootHint_=Delegation{delegationNS=d:|ds}} <- newEmptyEnv
    pure $ foldr takeV6 [] $ d:ds
  where
    nlist (x:|xs) = x:xs
    ax _   _ n6 xs = [(IPv6 a, 53) | a      <- nlist n6]  ++ xs
    a6 _     n6 xs = [(IPv6 a, 53) | a      <- nlist n6]  ++ xs
    takeV6 = delegationEntry ax (\_ _ xs -> xs) a6 (\_ xs -> xs) (\_ xs -> xs) (\_ xs -> xs)
{- FOURMOLU_ENABLE -}

iterativeQuery
    :: (DNSMessage -> IO ())
    -> Log.PutLines IO
    -> (Question, QueryControls)
    -> Options
    -> PortNumber
    -- ^ The port to ask authoritative servers on.
    -> IO ()
iterativeQuery putLn putLines qq opts authPort = do
    env <- setup putLines opts authPort
    er <- resolve env (optSynthesis opts) qq
    case er of
        Left e -> print e
        Right msg -> putLn msg

setup :: Log.PutLines IO -> Options -> PortNumber -> IO Env
setup putLines opt@Options{..} authPort = do
    tcache <- newTimeCache
    let cacheConf = Cache.getDefaultStubConf (4 * 1024) 600 $ getTime tcache
    cacheOps <- Cache.newRRCacheOps cacheConf
    -- Where to start from and what to believe, which go together: a
    -- world of one's own has a root of its own and is signed by a key
    -- the real root has never heard of.  Left alone, both are the real
    -- ones.
    rootHint <- mapM readRootHint optRootHints
    anchors <- maybe (pure mempty) (readTrustAnchors . pure) optTrustAnchor
    let tmout = timeout 3000000
        setOps =
            setRRCacheOps cacheOps
                . setTimeCache tcache
                . setRootHint rootHint
                . maybe id (const $ setRootAnchor anchors) optTrustAnchor
    newEmptyEnv <&> \env0 ->
        (setOps env0)
            { shortLog_ = shortLog opt
            , logLines_ = putLines
            , disableV6NS_ = optDisableV6NS
            , authPort_ = authPort
            , timeout_ = tmout
            }

resolve :: Env -> Synthesis -> (Question, QueryControls) -> IO (Either String DNSMessage)
resolve env synth (q, ctl) = resolveResponseIterative env synth q ctl

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NumericUnderscores #-}

module DNS.Iterative.Query.Norec where

-- GHC packages
import qualified Control.Exception as E

-- other packages

-- dnsext-types
import DNS.Types

-- dnsext-utils
import DNS.WorkerStats

-- dnsext-do53
import DNS.Do53.Client (
    FlagOp (..),
    defaultResolveActions,
 )
import qualified DNS.Do53.Client as DNS
import DNS.Do53.Internal (
    ResolveActions (..),
    ResolveEnv (..),
    ResolveInfo (..),
    defaultResolveInfo,
 )
import qualified DNS.Do53.Internal as DNS

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.Do53Stub (udpTcpResolver1)
import DNS.Iterative.Query.SteppedWait (steppedWait)

{- FOURMOLU_DISABLE -}
norec :: MonadIO m => Env -> WorkerStatOP -> Bool -> NonEmpty Address -> Domain -> TYPE -> m (Either DNSError DNSMessage)
norec cxt wstat dnssecOK aservers name typ =
    liftIO $ steppedWait TimeoutExpired RetryLimitExceeded 250_000 actions
  where
    actions = [apair (tag ++ ".q1"), apair (tag ++ ".q2")]
    tag = let (a:|as) = aservers in show (a:as)
    apair tag_ = (tag_, getWithBStats >>= \asps -> blockingIO_ tag_ (action asps))
    action asps@(x:|xs) =
        E.bracket_ openTasks closeTasks (norec_ 500_000 cxt dnssecOK asps name typ)
      where
        openTasks   = setTasks wstat [bstat | (_, bstat) <- x:xs]
        closeTasks  = clearTasks wstat
    getWithBStats = mapM (\x -> (,) x <$> getBlockingStatOP) aservers
    blockingIO_ tag_ = blockingIO wstat $ show name ++ " " ++ show typ ++ ": " ++ tag_
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
{- Get the answer DNSMessage from the authoritative server.
   Note about flags in request to an authoritative server.
  * RD (Recursion Desired) must be 0 for request to authoritative server
  * EDNS must be enable for DNSSEC OK request -}
norec_ :: Int -> Env -> Bool -> NonEmpty (Address, BlockingStatOP) -> Domain -> TYPE -> IO (Either DNSError DNSMessage)
norec_ utimeout cxt dnssecOK asps name typ = do
    let riActions bstatOP =
            defaultResolveActions
                { ractionGenId        = idGen_ cxt
                , ractionGetTime      = currentSeconds_ cxt
                , ractionLog          = logLines_ cxt
                , ractionShortLog     = shortLog_ cxt
                , ractionBlockingStat = bstatOP
                , ractionTimeoutTime  = utimeout
                }
        ris =
            [ defaultResolveInfo
                { rinfoIP        = aserver
                , rinfoPort      = port
                , rinfoActions   = riActions bstatOP
                , rinfoUDPRetry  = 1
                , rinfoVCLimit   = 8 * 1024
                }
            | ((aserver, port), bstatOP) <- asps
            ]
        renv =
            ResolveEnv
                { renvResolver      = udpTcpResolver1
                , renvConcurrent    = True -- should set True if multiple RIs are provided
                , renvResolveInfos  = ris
                }
        q = Question name typ IN
        doFlagSet
            | dnssecOK = FlagSet
            | otherwise = FlagClear
        qctl = DNS.rdFlag FlagClear <> DNS.doFlag doFlagSet
    fmap DNS.replyDNSMessage <$> DNS.resolve renv q qctl
{- FOURMOLU_ENABLE -}

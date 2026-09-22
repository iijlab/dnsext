{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NumericUnderscores #-}

module DNS.Iterative.Query.Norec where

-- GHC packages
import Control.Concurrent (myThreadId)
import Control.Exception (bracket_)

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
    ResolveEnv (..),
    ResolveActions (..),
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
    liftIO $ blockingIO wstat "norec" $ bracket_ setThId closeTasks body
  where
    body = steppedWait wstat TimeoutExpired RetryLimitExceeded waitInterval axs
    axs = [(tag ++ ".q1", action), (tag ++ ".q2", action)]
    tag = let (a:|as) = aservers in show name ++ " " ++ show typ ++ ": " ++ show (a:as)
    action = debugDelay >> norec_ 500_000 cxt wstat dnssecOK aservers name typ
    setThId = setThreadId wstat =<< myThreadId
    closeTasks  = clearTasks wstat
    waitInterval = 250_000
    debugDelay   = return ()
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
{- Get the answer DNSMessage from the authoritative server.
   Note about flags in request to an authoritative server.
  * RD (Recursion Desired) must be 0 for request to authoritative server
  * EDNS must be enable for DNSSEC OK request -}
norec_
    :: Int -> Env -> WorkerStatOP -> Bool -> NonEmpty Address
    -> Domain -> TYPE -> IO (Either DNSError DNSMessage)
norec_ utimeout cxt wstat dnssecOK aservers name typ = do
    asps@(x:|xs) <- sequence [ (,) x <$> getBlockingStatOP | x <- aservers ]
    addTasks wstat [bstat | (_, bstat) <- x:xs]
    {- The case of the name is mixed once for this query and every
       server it is put to, so that an answer has to repeat the mixture
       to be believed.  Where nothing is mixing it, the name goes as it
       stands and the case which comes back is nobody's business. -}
    name' <- maybe (pure name) ($ name) (mixCase_ cxt)
    let riActions bstatOP =
            defaultResolveActions
                { ractionGenId        = idGen_ cxt
                , ractionMixCase      = mixCase_ cxt
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
        q = Question name' typ IN
        doFlagSet
            | dnssecOK = FlagSet
            | otherwise = FlagClear
        qctl = DNS.rdFlag FlagClear <> DNS.doFlag doFlagSet
    fmap DNS.replyDNSMessage <$> DNS.resolve renv q qctl
{- FOURMOLU_ENABLE -}

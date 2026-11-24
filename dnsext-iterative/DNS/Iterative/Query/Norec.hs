{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NumericUnderscores #-}

module DNS.Iterative.Query.Norec where

-- GHC packages
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException (..), bracket_, catch, fromException, throwIO, try)
import Data.Foldable (toList)

-- other packages

-- dnsext-types
import DNS.Types

-- dnsext-utils
import qualified DNS.Log as Log
import qualified DNS.ThreadStats as TStat
import DNS.WorkerStats

-- dnsext-do53
import DNS.Do53.Client (
    FlagOp (..),
    defaultResolveActions,
 )
import qualified DNS.Do53.Client as DNS
import DNS.Do53.Internal (
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
    action giveUp = debugDelay >> norec_ giveUp 500_000 cxt wstat dnssecOK aservers name typ
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
    :: STM () -> Int -> Env -> WorkerStatOP -> Bool -> NonEmpty Address
    -> Domain -> TYPE -> IO (Either DNSError DNSMessage)
norec_ giveUp utimeout cxt wstat dnssecOK aservers name typ = do
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
        q = Question name' typ IN
        doFlagSet
            | dnssecOK = FlagSet
            | otherwise = FlagClear
        qctl = DNS.rdFlag FlagClear <> DNS.doFlag doFlagSet
    fmap DNS.replyDNSMessage <$> raceResolve giveUp (toList ris) q qctl
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | Asking every server at once.  The first answer wins and the others
--   give up on it, as on the given 'STM'.  The last error is returned
--   when none answers.
raceResolve :: STM () -> [ResolveInfo] -> Question -> DNS.QueryControls -> IO (Either DNSError DNS.Reply)
raceResolve giveUp [ri] q qctl = resolve1 giveUp ri q qctl
raceResolve giveUp ris q@(Question qn qt _) qctl = do
    won <- newTVarIO False
    results <- newTQueueIO
    caller <- TStat.getThreadLabel
    let giveUp' = giveUp <|> (readTVar won >>= check)
        -- The thread's top: whatever ends it is recorded as data.
        orFailure = either (\se -> Left $ NetworkFailure se "do53-res") id
    sequence_
        [ TStat.forkIO (caller ++ ".do53-res: " ++ show (rinfoIP ri)) $
            try (resolve1 giveUp' ri q qctl) >>= atomically . writeTQueue results . (,) ri . orFailure
        | ri <- ris ]
    let collect :: Int -> IO (Either DNSError DNS.Reply)
        collect n = do
            (ri, r) <- atomically $ readTQueue results
            case r of
                Right rply -> do
                    atomically $ writeTVar won True
                    ractionLog (rinfoActions ri) Log.DEMO Nothing
                        ["    query @" ++ DNS.fromNameTag (DNS.replyTag rply) ++ " " ++ show qn ++ " " ++ show qt ++ ": win"]
                    return $ Right rply
                Left e
                    | n <= 1 -> return $ Left e
                    | otherwise -> collect (n - 1)
    collect $ length ris

-- | Asking a server, with its errors as data: thrown from the caller of
--   'steppedWait', a timeout reached the top of a thread nobody waits on.
resolve1 :: STM () -> ResolveInfo -> Question -> DNS.QueryControls -> IO (Either DNSError DNS.Reply)
resolve1 giveUp ri q@(Question qn qt _) qctl =
    (either (Left . context) Right <$> udpTcpResolver1 giveUp ri q qctl) `catch` \se ->
        case fromException se of
            Just (SomeAsyncException _) -> throwIO se
            Nothing -> return $ Left $ context $ fromMaybe (NetworkFailure se "") $ fromException se
  where
    context e = DNSErrorInfo e $ "query " ++ show qn ++ " " ++ show qt ++ " to " ++ show (rinfoIP ri) ++ "#" ++ show (rinfoPort ri)
{- FOURMOLU_ENABLE -}

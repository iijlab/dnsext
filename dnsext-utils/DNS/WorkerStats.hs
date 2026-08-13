{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module DNS.WorkerStats where

-- GHC packages
import qualified Control.Exception as E
import Data.IORef
import Data.List (sortBy)
import Data.Ord (comparing)

-- dnsext-* packages

import qualified DNS.ThreadStats as TStat
import DNS.Types (Question (..))
import DNS.Types.Time (EpochTimeUsec, diffUsec, getCurrentTimeUsec, runEpochTimeUsec)

-- this package
import DNS.Transport.Types (DoX (..))

{- FOURMOLU_DISABLE -}
pprWorkerStats :: Int -> [WorkerStatOP] -> IO [String]
pprWorkerStats _pn ops = do
    stats <- zip3 [1 :: Int ..] <$> mapM getBlockingStat ops <*> mapM pprTasks ops
    let isBkStat p (_n, (_ctx, bks, _cause, _diff), _ts) = p bks
        ablockings  = filter (isBkStat (== StatBlocking))  stats
        runnings    = filter (isBkStat (== StatUnblocked)) stats
        isBkCause p (_n, (_ctx, _bks, cause, _diff), _ts) = p cause
        requests    = filter (isBkCause (== CauseRequest))  ablockings
        responses   = filter (isBkCause (== CauseResponse)) ablockings
        blockings   = filter (isBkCause (`notElem` [CauseRequest, CauseResponse])) ablockings
        {- sorted by query span -}
        getDiffT (_n, (_bks, _ctx, _cause, diff), _ts) = diff
        sorted = sortBy (comparing $ (\(DiffT int) -> int) . getDiffT) $ runnings ++ blockings
        pprEnq  p (wn, wbs@(ContextQuery dox _q, _, _, _), _ts)
            | p dox  = ((show wn ++ ":" ++ pprBlkStat wbs) :)
        pprEnq _p  _  = id
        pprEnqs
            | null pp    = "no workers"
            | otherwise  = pp
          where h2  = foldr (pprEnq (== H2))  [] responses
                dot = foldr (pprEnq (== DoT)) [] responses
                xs  = foldr (pprEnq (\x -> x /= H2 && x /= DoT)) [] responses
                pp = unwords (h2 ++ dot ++ xs)

        pprq (wn, bks, ppts) = [showDec3 wn ++ ": " ++ pprBlkStat bks] ++ ppts
        pprdeq = " waiting dequeues: " ++ show (length requests) ++ " workers"
        pprenq = " waiting enqueues: " ++ pprEnqs

    return $ concatMap pprq sorted ++ [pprdeq, pprenq]
  where
    pprBlkStat (context, bstate, cause, diff) = pprCtxBlockingStat context bstate cause diff
    pprTasks op = getTasks op >>= mapM pprTask
    pprTask bs = withBlockingStat bs $ \bstat cause dtime -> return (pprTaskBlockingStat bstat cause dtime)
    showDec3 n
        | 100 <= n   = show n
        | 10  <= n   = ' ' : show n
        | otherwise  = "  " ++ show n
{- FOURMOLU_ENABLE -}

------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data BlockingStat
    = StatBlocking
    | StatUnblocked
    deriving Eq

instance Show BlockingStat where
    show StatBlocking   = " blocking"
    show StatUnblocked  = "unblocked"

data BlockingCause
    = CauseUndef
    | CauseRequest
    | CauseResponse
    | CauseEnqueue  String
    | CauseLog      String
    | CauseIO       String
    deriving Eq

instance Show BlockingCause where
    show  CauseUndef           = "<blocking cause unused>"
    show  CauseRequest         = "dequeue: request"
    show  CauseResponse        = "enqueue: response"
    show (CauseEnqueue  note)  = "enqueue: " ++ note
    show (CauseLog      note)  = "logging: " ++ note
    show (CauseIO       note)  = "I/O: " ++ note

-- |
--  BlockingContext transition in worker/cacher
--
--   +-----------+        +-----------+
--   |  Request  | -----> |   Query   |
--   +-----------+        +-----+-----+
--         ^                    |
--         | cacher/worker loop |
--         +--------------------+
--
--   --------------------------------------------------------------
--
--   e.g. internal transition under CauseRequest
--   +------------------------------------------+
--   |  CauseRequest                            |
--   |                                          |
--   |  BlockingStat transition                 |
--   |    +-----------+       +-----------+     |
--   |    | Blocking  | ----> | Unblocked |     |
--   |    +-----------+       +-----------+     |
--   |                                          |
--   +------------------------------------------+
--
--   e.g. internal transition under (CauseEnqueue "dnstap")
--   +------------------------------------------+
--   |  CauseEnqueue "dnstap"                   |
--   |                                          |
--   |  BlockingStat transition                 |
--   |    +-----------+       +-----------+     |
--   |    | Blocking  | ----> | Unblocked |     |
--   |    +-----------+       +-----------+     |
--   |                                          |
--   +------------------------------------------+
--
data BlockingContext
    = ContextRequest
    | ContextQuery DoX Question
    deriving Eq

-- |
-- >>> import Data.String
-- >>> import DNS.Types
-- >>> ContextQuery DoT (Question (fromString "example.com") A IN)
-- DoT: "example.com." A IN
instance Show BlockingContext where
    show  ContextRequest       = ""
    show (ContextQuery dox q)  = show dox ++ ": " ++ showQ q
      where
        showQ (Question qn ty cls) = unwords [show qn, show ty, show cls]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
pprBlockingStat :: Int -> String -> BlockingStat -> BlockingCause -> DiffTime -> String
pprBlockingStat pwidth ctx bstate cause diff =
    pad ++ diffStr ++ ": " ++ show bstate ++ npp ctx ++ ": " ++ show cause
  where
    diffStr = showDiffSec1 diff
    pad = replicate (pwidth - length diffStr) ' '
    npp s
        | null s     = ""
        | otherwise  = ": " ++ s
{- FOURMOLU_ENABLE -}

pprTaskBlockingStat :: BlockingStat -> BlockingCause -> DiffTime -> String
pprTaskBlockingStat = pprBlockingStat 9 ""

pprCtxBlockingStat :: BlockingContext -> BlockingStat -> BlockingCause -> DiffTime -> String
pprCtxBlockingStat context =
    pprBlockingStat pwidth (show context)
  where
    pwidth = 7

------------------------------------------------------------

class OpBlockingStat op where
    setBlocking :: op -> BlockingCause -> IO ()
    setUnblocked :: op -> IO ()
    withBlockingStat :: op -> (BlockingStat -> BlockingCause -> DiffTime -> IO a) -> IO a

------------------------------------------------------------

data BlockingStatOP
    = BlockingStatOP
    { setBlocking_ :: BlockingCause -> IO ()
    , setUnblocked_ :: IO ()
    , withBlockingStat_ :: forall a. (BlockingStat -> BlockingCause -> DiffTime -> IO a) -> IO a
    }

instance OpBlockingStat BlockingStatOP where
    setBlocking = setBlocking_
    setUnblocked = setUnblocked_
    withBlockingStat = withBlockingStat_

{- FOURMOLU_DISABLE -}
data WorkerStatOP =
    WorkerStatOP
    { setQuery     :: DoX -> Question -> IO ()
    , setRequest   :: IO ()
    , withContext  :: forall a . (BlockingContext -> IO a) -> IO a
    , blockingOP   :: BlockingStatOP
    , setTasks     :: [BlockingStatOP] -> IO ()
    , getTasks     :: IO [BlockingStatOP]
    , clearTasks   :: IO ()
    }

instance OpBlockingStat WorkerStatOP where
    setBlocking       = setBlocking_ . blockingOP
    setUnblocked      = setUnblocked_ . blockingOP
    withBlockingStat  = withBlockingStat_ . blockingOP
{- FOURMOLU_ENABLE -}

getBlockingStat :: WorkerStatOP -> IO (BlockingContext, BlockingStat, BlockingCause, DiffTime)
getBlockingStat op = withContext op $ \cx -> withBlockingStat_ (blockingOP op) $ \bs bc dt -> return (cx, bs, bc, dt)

data WBStatStore = WBStatStore BlockingStat TimeStamp

{- FOURMOLU_DISABLE -}
data WBlockingStore =
    WBStore
    { wbkStatRef  :: IORef WBStatStore  -- consistently access, stat and timestamp pair
    , wbkCause    :: BlockingCause
    }
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
noopBlockingStat :: BlockingStatOP
noopBlockingStat =
    BlockingStatOP
    { setBlocking_       = \_ -> return ()
    , setUnblocked_      = return ()
    , withBlockingStat_  = \k -> k StatBlocking CauseUndef (DiffT (-1))
    }
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
noopWorkerStat :: WorkerStatOP
noopWorkerStat =
    WorkerStatOP
    { setQuery         = \_ _ -> return ()
    , setRequest       = return ()
    , withContext      = \k -> k ContextRequest
    , blockingOP       = noopBlockingStat
    , setTasks         = \_ -> return ()
    , getTasks         = return []
    , clearTasks       = return ()
    }
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getBlockingStatOP :: IO BlockingStatOP
getBlockingStatOP = do
    blkRef  <- newIORef =<< newBlkStore CauseUndef
    return
        BlockingStatOP
        { setBlocking_       = blocking  blkRef
        , setUnblocked_      = unblocked blkRef
        , withBlockingStat_  = withBlkStat blkRef
        }
  where
    mkBsStore bstat = WBStatStore bstat <$> getTimeStamp
    newBlkStore cause = do
        ref <- newIORef =<< mkBsStore StatBlocking
        return WBStore{wbkStatRef = ref, wbkCause = cause}
    blocking blkRef cause = do
        store <- newBlkStore cause
        writeIORef blkRef store
    unblocked bkRef = do
        WBStore{wbkStatRef = ref} <- readIORef bkRef
        writeIORef ref =<< mkBsStore StatUnblocked
    withBlkStat blkRef = \k -> do
        WBStore{wbkStatRef = ref, wbkCause = cause} <- readIORef blkRef
        WBStatStore bstat ts0 <- readIORef ref
        now <- getTimeStamp
        k bstat cause (now `diffTimeStamp` ts0)

getWorkerStatOP :: IO WorkerStatOP
getWorkerStatOP = do
    ctxRef  <- newIORef     ContextRequest
    blkOp   <- getBlockingStatOP
    tskRef  <- newIORef     []
    return
        WorkerStatOP
        { setQuery         = \dox q -> writeIORef ctxRef $ ContextQuery dox q
        , setRequest       = writeIORef ctxRef ContextRequest
        , withContext      = \k -> readIORef ctxRef >>= k
        , blockingOP       = blkOp
        , setTasks         = writeIORef tskRef
        , getTasks         = readIORef tskRef
        , clearTasks       = writeIORef tskRef []
        }
{- FOURMOLU_ENABLE -}

contextSetQuery :: WorkerStatOP -> DoX -> Question -> IO ()
contextSetQuery = setQuery

contextClear :: WorkerStatOP -> IO ()
contextClear = setRequest

{- FOURMOLU_DISABLE -}
eventLogWS :: WorkerStatOP -> IO ()
eventLogWS wstat = withContext wstat $ \context -> withBlockingStat wstat $ \bstate cause diff -> do
    let wspp = pprCtxBlockingStat context bstate cause diff
    TStat.eventLog $ "iter.st " ++ wspp
{- FOURMOLU_ENABLE -}

bracketBlocking :: OpBlockingStat op => op -> BlockingCause -> IO a -> IO a
bracketBlocking wstat cause = E.bracket_ (setBlocking wstat cause) (setUnblocked wstat)

blockingRequest :: WorkerStatOP -> IO a -> IO a
blockingRequest wstat x = bracketBlocking wstat CauseRequest (eventLogWS wstat >> x)

blockingResponse :: WorkerStatOP -> IO a -> IO a
blockingResponse wstat x = bracketBlocking wstat CauseResponse (eventLogWS wstat >> x)

blockingEnqueue :: WorkerStatOP -> String -> IO a -> IO a
blockingEnqueue wstat note x = bracketBlocking wstat (CauseEnqueue note) (eventLogWS wstat >> x)

blockingLog :: OpBlockingStat op => op -> String -> IO a -> IO a
blockingLog wstat note = bracketBlocking wstat (CauseLog note)

blockingIO :: OpBlockingStat op => op -> String -> IO a -> IO a
blockingIO wstat note = bracketBlocking wstat (CauseIO note)

------------------------------------------------------------

type TimeStamp = EpochTimeUsec
newtype DiffTime = DiffT Integer

getTimeStamp :: IO TimeStamp
getTimeStamp = getCurrentTimeUsec

toMicrosec :: TimeStamp -> Integer
toMicrosec eus = runEpochTimeUsec eus toMicro
  where
    toMicro sec micro = fromIntegral sec * microf + fromIntegral micro
    microf = 1_000_000

diffTimeStamp :: TimeStamp -> TimeStamp -> DiffTime
diffTimeStamp t1 t2 = DiffT (diffUsec t1 t2)

{- FOURMOLU_DISABLE -}
showDiffSec1 :: DiffTime -> String
showDiffSec1 (DiffT susec)
    | susec < 0  = '-' : str ++ "s"
    | otherwise  = str ++ "s"
  where
    usec = abs susec
    df = 100_000
    dsec = usec `quot` df
    (sec, d) = dsec `quotRem` 10
    str = show sec ++ "." ++ show d
{- FOURMOLU_ENABLE -}

{-# LANGUAGE NumericUnderscores #-}

module DNS.Iterative.Server.WorkerStats where

-- GHC packages
import Data.IORef
import Data.List (sortBy)
import Data.Ord (comparing)

-- dnsext-* packages
import DNS.Types (Question (..))
import DNS.Types.Time (EpochTimeUsec, diffUsec, getCurrentTimeUsec, runEpochTimeUsec)

-- this package
import DNS.Iterative.Server.Types (DoX (..))

{- FOURMOLU_DISABLE -}
pprWorkerStats :: Int -> [WorkerStatOP] -> IO [String]
pprWorkerStats _pn ops = do
    stats <- zip [1 :: Int ..] <$> mapM getWorkerStat ops
    let isStat p = p . fst . snd
        isEnqueue WWaitEnqueue{}  = True
        isEnqueue _               = False
        qs = filter (isStat ((&&) <$> (/= WWaitDequeue) <*> not . isEnqueue)) stats
        {- sorted by query span -}
        sorted = sortBy (comparing $ (\(DiffT int) -> int) . snd . snd) qs
        deqs = filter (isStat (== WWaitDequeue)) stats
        pprEnq  p (wn, (WWaitEnqueue _qs dox tg, ds))
            | p dox  = ((show wn ++ ":" ++ show dox ++ ":" ++ show tg ++ ":" ++ showDiffSec1 ds) :)
        pprEnq _p  _  = id
        pprEnqs
            | null pp    = "no workers"
            | otherwise  = pp
          where h2  = foldr (pprEnq (== H2))  [] stats
                dot = foldr (pprEnq (== DoT)) [] stats
                xs  = foldr (pprEnq (\x -> x /= H2 && x /= DoT)) [] stats
                pp = unwords (h2 ++ dot ++ xs)

        pprq (wn, st) = showDec3 wn ++ ": " ++ pprWorkerStat st
        pprdeq = " waiting dequeues: " ++ show (length deqs) ++ " workers"
        pprenq = " waiting enqueues: " ++ pprEnqs

    return $ map pprq sorted ++ [pprdeq, pprenq]
  where
    showDec3 n
        | 100 <= n   = show n
        | 10  <= n   = ' ' : show n
        | otherwise  = "  " ++ show n
{- FOURMOLU_ENABLE -}

pprWorkerStat :: (WorkerStat, DiffTime) -> String
pprWorkerStat (stat, diff) = pad ++ diffStr ++ ": " ++ show stat
  where
    diffStr = showDiffSec1 diff
    pad = replicate (width - length diffStr) ' '
    width = 7

------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data EnqueueTarget
    = EnBegin
    | EnCCase String  -- for CacheResult cases
    | EnTap
    | EnSend
    | EnEnd
    deriving Eq

instance Show EnqueueTarget where
    show  EnBegin     = "Bgn"
    show (EnCCase s)  = "CCase " ++ s
    show  EnTap       = "Tap"
    show  EnSend      = "Send"
    show  EnEnd       = "End"

data WorkerStat
    = WWaitDequeue
    | WRun Question
    | WWaitEnqueue Question DoX EnqueueTarget
    deriving Eq

instance Show WorkerStat where
    show st = case st of
        WWaitDequeue                 -> "waiting dequeue - WWaitDequeue"
        WRun q                       -> "querying" ++ pprQ q ++ " - WRun"
        WWaitEnqueue q dox tg        -> "waiting enqueue" ++ pprQ q ++ " " ++ show dox ++ " " ++ show tg ++ " - WWaitEnqueue"
      where pprQ (Question n t c) = " " ++ show n ++ " " ++  show t ++ " " ++ show c
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
data WorkerStatOP =
    WorkerStatOP
    { setWorkerStat :: WorkerStat -> IO ()
    , getWorkerStat :: IO (WorkerStat, DiffTime)
    }
{- FOURMOLU_ENABLE -}

data WStatStore = WSStore WorkerStat TimeStamp

getWorkerStatOP :: IO WorkerStatOP
getWorkerStatOP = do
    ref <- newIORef =<< getStore WWaitDequeue
    pure $ WorkerStatOP (setStat ref) (getStat ref)
  where
    getStore stat = WSStore stat <$> getTimeStamp
    setStat ref stat = writeIORef ref =<< getStore stat
    getStat ref = do
        WSStore s ts0 <- readIORef ref
        now <- getTimeStamp
        return (s, now `diffTimeStamp` ts0)

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

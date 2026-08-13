{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DNS.Iterative.Query.SteppedWait (
    steppedWait,
) where

-- ghc internal
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)

-- ghc
import Control.Applicative
import Control.Concurrent hiding (forkIO)
import Control.Concurrent.STM
import Control.Exception (AsyncException (..))
import qualified Control.Exception as E
import Control.Monad
import Data.Functor

-- dnsext-*
import DNS.ThreadStats (forkIO)
import DNS.Types (DNSError (NetworkFailure))

-- $setup
-- >>> :seti -XNumericUnderscores

{- FOURMOLU_DISABLE -}
-- try function for doctests
_tryDNS :: IO a -> IO (Either DNSError a)
_tryDNS action = either left right =<< E.try action
  where
    right x = pure (Right x)
    left ex
      -- -- | Just ae <- fromException ex :: Maybe AsyncCancelled  = throwIO ae
      | Just ae <- E.fromException ex :: Maybe AsyncException = E.throwIO ae
      | Just de <- E.fromException ex :: Maybe DNSError       = pure (Left de)
      | otherwise                                             = pure (Left $ NetworkFailure ex "")
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- |
-- >>> import DNS.Types (DNSError (TimeoutExpired, RetryLimitExceeded, UnknownDNSError))
-- >>> steppedWait' uu actions = steppedWait TimeoutExpired RetryLimitExceeded uu (zip (cycle [""]) $ map _tryDNS actions)
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer     remain-actions
--
-- 0    []         noTimer   []
--
-- EvNoRunning
--   Left RetryLimitExceeded
--
-- >>> steppedWait' 100_000 [] :: IO (Either DNSError ())
-- Left RetryLimitExceeded
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer     remain-actions
--
-- 0    []         noTimer   [A]
--
-- EvNoRunning
--   fork A ...
--
-- 1    [A]        A         []
--
-- EvTimer
--   Left TimeoutExpired
--
-- >>> steppedWait' 100_000 [threadDelay 1_000_000]
-- Left TimeoutExpired
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer   remain-actions
--
-- 0    []         retry   [A]
--
-- EvNoRunning
--   fork A
--
-- 1    [A]        A       []
--
-- EvResult (Right x)
--   Right x
--
-- >>> steppedWait' 1_000_000 [threadDelay 100_000 $> '1']
-- Right '1'
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer   remain-actions
--
-- 0    []         retry   [A]
--
-- EvNoRunning
--   fork A
--
-- 1    [A]        A       []
--
-- EvResult (Left e)
--   waitEV
--
-- 0    []         A       []
--
-- EvNoRunning
--   Left <lastE>
--
-- >>> steppedWait' 1_000_000 [threadDelay 100_000 >> E.throwIO UnknownDNSError] :: IO (Either DNSError ())
-- Left UnknownDNSError
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer   remain-actions
--
-- 0    []         retry   [A,B]
--
-- EvNoRunning
--   fork A
--
-- 1    [A]        A       [B]
--
-- EvTimer
--   fork B
--
-- 2    [A,B]      B       []
--
-- EvResult (Left e)
--   waitEV
--
-- 1    [B]        B       []
--
-- EvResult (Right x)
--   Right x
--
-- >>> steppedWait' 150_000 [threadDelay 200_000 >> E.throwIO UnknownDNSError, threadDelay 100_000 $> '2']
-- Right '2'
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer   remain-actions
--
-- 0    []         retry   [A,B]
--
-- EvNoRunning
--   fork A
--
-- 1    [A]        A       [B]
--
-- EvResult (Left e)
--   waitEV
--
-- 0    []         A       [B]
--
-- EvNoRunning
--   fork B
--
-- 1    [B]        B       []
--
-- EvResult (Right x)
--   Right x
--
-- >>> steppedWait' 1_000_000 [threadDelay 100_000 >> E.throwIO UnknownDNSError, threadDelay 100_000 $> '3']
-- Right '3'
--
-- --------------------------------------------------------------------------------
-- run  runnings   timer   remain-actions
--
-- 0    []         retry   [A,B]
--
-- EvNoRunning
--   fork A
--
-- 1    [A]        A       [B]
--
-- EvTimer
--   fork B
--
-- 2    [A,B]      B       []
--
-- EvResult (Left e)
--   waitEV
--
-- 1    [A]        B       []
--
-- EvResult (Right x)
--   Right x
--
-- >>> steppedWait' 150_000 [threadDelay 250_000 $> '4', threadDelay 50_000 >> E.throwIO UnknownDNSError]
-- Right '4'
--
steppedWait
  :: Show a
  => e -> e -> Int
  -> [(String, IO (Either e a))] -> IO (Either e a)
steppedWait exTimeout exNoResult uusec actions = do
  vrun <- newTVarIO 0
  vrem <- newTVarIO $ length actions
  qres <- newTQueueIO
  let noTimer = Timer{ timSTM_ = retry, timDel_ = pure ()}
  steppedWaitLoop vrun vrem qres exTimeout uusec noTimer id exNoResult actions
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
steppedWaitLoop
  :: Show a
  => TVar Running
  -> TVar Int -> TQueue (Either e a)
  -> e -> Int -> Timer
  -> ([ThreadId] -> [ThreadId]) -> e -> [(String, IO (Either e a))]
  -> IO (Either e a)
steppedWaitLoop vrun vrem qres exTimeout uusec timer0 tids lastE0 xxs = eventLoop timer0 lastE0
  where
    nexts lastE x xs timer = fork x >>= \tid -> steppedWaitLoop vrun vrem qres exTimeout uusec timer (tids . (tid:)) lastE xs

    fork x = doFork vrun qres x
    waitEV timer = waitEvent vrem qres vrun timer

    eventLoop timer lastE = waitEV timer >>= dispatchEV timer lastE
    dispatchEV timer lastE ev = do
        case ev of
          EvNoRunning        -> case xxs of
            []                 -> finalize   $>   Left lastE
            x:xs               -> renewTimer >>=  nexts lastE x xs
          EvTimer            -> case xxs of
            []                 -> finalize   $>   Left exTimeout
            x:xs               -> renewTimer >>=  nexts lastE x xs
          EvNoResult           -> finalize   $>   Left lastE
          EvResult  (Left e)   ->                 eventLoop timer e
          EvResult r@Right{}   -> finalize   $>   r
      where
        renewTimer  = timDel_ timer >> newTimer uusec
        finalize    = timDel_ timer >> mapM_ killThread (tids [])
{- FOURMOLU_ENABLE -}

--------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data Timer =
  Timer
  { timSTM_   :: STM ()
  , timDel_   :: IO ()
  -- , timMgr_   :: TimerManager
  -- , timKey_   :: TimeoutKey
  }

newTimer :: Int -> IO Timer
newTimer usec = do
    var  <- newEmptyTMVarIO
    mgr  <- getSystemTimerManager
    key  <- registerTimeout mgr usec (atomically $ putTMVar var ())
    pure Timer{timSTM_ = takeTMVar var, timDel_  = unregisterTimeout mgr key}
{- FOURMOLU_ENABLE -}

--------------------------------------------------------------------------------

newtype Running = Running Int deriving (Eq, Ord, Num, Show)

{- FOURMOLU_DISABLE -}
doFork
  :: TVar Running -> TQueue (Either e a)
  -> (String, IO (Either e a)) -> IO ThreadId
doFork vrun qres (label, x) = do
    let bgn = atomically $ modifyTVar vrun (+ 1)
        end = atomically $ modifyTVar vrun (subtract 1)
    bgn >> forkIO label (E.finally (x >>= \e -> atomically $ writeTQueue qres e) end)
{- FOURMOLU_ENABLE -}

--------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data Event e a
  = EvNoRunning
  | EvTimer
  | EvNoResult
  | EvResult (Either e a)
  deriving Show

waitQueueEventSTM
  :: TVar Int
  -> TQueue (Either e a)
  -> STM (Event e a)
waitQueueEventSTM vrem qres = do
  remain <- readTVar vrem  -- count of not finished actions
  if remain > 0
    then readTQueue qres >>= \x -> writeTVar vrem (remain - 1) $> EvResult x
    else                                                     pure EvNoResult

waitEventSTM
  :: TVar Int -> TQueue (Either e a)
  -> TVar Running -> Timer
  -> STM (Event e a)
waitEventSTM vrem qres vrun timer =
    waitQueueEventSTM vrem qres
    <|>
    noRunning      $> EvNoRunning
    <|>
    timSTM_ timer  $> EvTimer
  where
    noRunning = readTVar vrun >>= \run -> guard (run <= 0)
{- FOURMOLU_ENABLE -}

waitEvent
    :: TVar Int
    -> TQueue (Either e a)
    -> TVar Running
    -> Timer
    -> IO (Event e a)
waitEvent vrem qres vrun timer = atomically $ waitEventSTM vrem qres vrun timer

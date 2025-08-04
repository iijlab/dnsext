{-# LANGUAGE CPP #-}

module DNS.ThreadStats where

#if __GLASGOW_HASKELL__ >= 906

-- GHC internal
import GHC.Conc.Sync (threadStatus)
import qualified GHC.Conc.Sync as GHC

-- base
import Control.Concurrent (myThreadId)

#else

-- (imports for case, GHC 9.4.x, GHC 9.2.x)

#endif

-- GHC internal
import GHC.Conc.Sync (labelThread)

-- base
import Control.Concurrent (ThreadId, threadDelay)
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async (Async, asyncThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Monad
import Data.List
import Data.Maybe

showTid :: ThreadId -> String
showTid tid = stripTh $ show tid
  where
    stripTh x = fromMaybe x $ stripPrefix "ThreadId " x

---

getThreadLabel :: IO String
dumpThreads :: IO [String]
dumper :: ([String] -> IO ()) -> IO ()

#if __GLASGOW_HASKELL__ >= 906

getThreadLabel = withName (pure "<no-label>") $ \tid n -> pure $ n ++ ": " ++ showTid tid
  where
    withName nothing just = do
        tid <- myThreadId
        maybe nothing (just tid) =<< GHC.threadLabel tid

dumpThreads = do
    ts <- mapM getName =<< GHC.listThreads
    vs <- sequence [ dump tid n | (tid, Just n)  <- ts ]
    pure . map (uncurry (++)) $ sort vs
  where
    getName tid = (,) tid <$> GHC.threadLabel tid
    dump tid name = do
        st <- show <$> threadStatus tid
        let stid = showTid tid
            pad = replicate (width - length name - length stid) ' '
            val = pad ++ ": " ++ stid ++ ": " ++ st
        pure (name, val)
    width = 24

dumper putLines = forever $ do
    putLines . (++ ["----------------------------------------"]) =<< dumpThreads
    threadDelay interval
  where
    interval = 3 * 1000 * 1000

#else

getThreadLabel = pure "<thread-label not supported>"
dumpThreads = pure ["<not supported>"]
dumper _ = forever $ threadDelay interval
  where
    interval = 3 * 1000 * 1000

#endif

---

listThreads :: IO [ThreadId]
threadLabel :: ThreadId -> IO (Maybe String)

#if __GLASGOW_HASKELL__ >= 906

listThreads = GHC.listThreads
threadLabel = GHC.threadLabel

#else

listThreads = pure []
threadLabel _ = pure Nothing

#endif

---

{- FOURMOLU_DISABLE -}
forkIO :: String -> IO () -> IO ThreadId
async :: String -> IO a -> IO (Async a)
withAsync :: String -> IO a -> (Async a -> IO b) -> IO b
withAsyncs :: [(String, IO a)] -> ([Async a] -> IO b) -> IO b
concurrently :: String -> IO a -> String -> IO b -> IO (a, b)
concurrently_ :: String -> IO a -> String -> IO b -> IO ()
race :: String -> IO a -> String -> IO b -> IO (Either a b)
race_ :: String -> IO a -> String -> IO b -> IO ()
concurrentlyList :: [(String, IO a)] -> IO [a]
concurrentlyList_ :: [(String, IO a)] -> IO ()
raceList :: [(String, IO a)] -> IO (Async a, a)
raceList_ :: [(String, IO a)] -> IO ()
{- FOURMOLU_ENABLE -}

forkIO name action = do
    tid <- Concurrent.forkIO action
    labelThread tid name
    pure tid

async name io = do
    a <- Async.async io
    labelThread (asyncThreadId a) name
    pure a

withAsync name io h0 =
    Async.withAsync io h
  where
    h a = do
        labelThread (asyncThreadId a) name
        h0 a

withAsyncs ps h = foldr op (\f -> h (f [])) ps id
  where
    op (n, io) action = \s -> withAsync n io $ \a -> action (s . (a :))

{- FOURMOLU_DISABLE -}
concurrently nleft left nright right =
    withAsync nleft  left $ \a ->
    withAsync nright right $ \b ->
    Async.waitBoth a b
{- FOURMOLU_ENABLE -}

concurrently_ nleft left nright right = void $ concurrently nleft left nright right

{- FOURMOLU_DISABLE -}
race nleft left nright right =
    withAsync nleft  left $ \a ->
    withAsync nright right $ \b ->
    Async.waitEither a b
{- FOURMOLU_ENABLE -}

race_ nleft left nright right = void $ race nleft left nright right

-- |
-- >>> concurrentlyList $ zip [[c] | c <- ['a'..]] [pure x | x <- [1::Int .. 5]]
-- [1,2,3,4,5]
concurrentlyList ps = withAsyncs ps $ mapM Async.wait

concurrentlyList_ = void . concurrentlyList

raceList ps = withAsyncs ps Async.waitAny

raceList_ = void . raceList

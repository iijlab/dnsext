{-# LANGUAGE CPP #-}

module DNS.ThreadStats where

#if __GLASGOW_HASKELL__ >= 906

-- GHC internal
import GHC.Conc.Sync (threadStatus)
import qualified GHC.Conc.Sync as GHC

#else

-- (imports for case, GHC 9.4.x, GHC 9.2.x)

#endif

-- GHC internal
import GHC.Conc.Sync (labelThread)

-- base
import Control.Concurrent (ThreadId, myThreadId)
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace (traceEventIO)

showTid :: ThreadId -> String
showTid tid = stripTh $ show tid
  where
    stripTh x = fromMaybe x $ stripPrefix "ThreadId " x

---

eventLog :: String -> IO ()
eventLog s = do
    tid <- showTid <$> myThreadId
    traceEventIO ("uevent: thread " ++ tid ++ " (" ++ s ++ ")")

-- naming not named
setThreadLabel :: String -> IO ()
setThreadLabel name = do
    tid <- myThreadId
    maybe (labelThread tid name) (const $ pure ()) =<< threadLabel tid

---

getThreadLabel :: IO String
dumpThreads :: IO [String]
-- | Dumping the threads every 3 seconds until the 'STM' succeeds.
dumper :: STM () -> ([String] -> IO ()) -> IO ()

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

dumper stop putLines = loop
  where
    loop = do
        putLines . (++ ["----------------------------------------"]) =<< dumpThreads
        stopped <- stoppedWithin (3 * 1000 * 1000) stop
        unless stopped loop

#else

getThreadLabel = pure "<thread-label not supported>"
dumpThreads = pure ["<not supported>"]
dumper stop _ = void $ atomically stop

#endif

---

-- | Waiting for the interval or for the 'STM': 'True' for the latter.
stoppedWithin :: Int -> STM () -> IO Bool
stoppedWithin usec stop = do
    late <- registerDelay usec
    atomically $ (True <$ stop) `orElse` (False <$ (readTVar late >>= check))

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

forkIO :: String -> IO () -> IO ThreadId
forkIO name action = do
    tid <- Concurrent.forkIO action
    labelThread tid name
    pure tid

-- | Stopping a server by telling it, and waiting for the sessions it started.
module DNS.Iterative.Server.Stop (
    stoppable,
    stopWithoutWaiting,
    Sessions,
    newSessions,
    withSession,
    waitSessions,
) where

-- GHC packages
import Control.Concurrent.Async (waitCatchSTM)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (join, void)

-- dnsext-utils packages
import qualified DNS.ThreadAsync as TAsync

-- | Running a server with an installer of the action which stops it.
--   The installed action is run once the 'STM' action fires.
stoppable :: STM () -> ((IO () -> IO ()) -> IO a) -> IO a
stoppable stop server = do
    var <- newEmptyMVar
    TAsync.withAsync "bw.stopper" (atomically stop >> join (readMVar var)) $ \_ ->
        server $ void . tryPutMVar var

-- | Running a server until the 'STM' action fires, then telling it to stop
--   and returning without waiting for it to wind down.
stopWithoutWaiting :: String -> STM () -> ((IO () -> IO ()) -> IO ()) -> IO ()
stopWithoutWaiting name stop server = do
    var <- newEmptyTMVarIO
    a <- TAsync.async name $ server $ atomically . void . tryPutTMVar var
    r <- atomically $ (Nothing <$ stop) `orElse` (Just <$> waitCatchSTM a)
    case r of
        Nothing -> join $ atomically $ takeTMVar var `orElse` (return () <$ waitCatchSTM a)
        Just e -> either E.throwIO return e

-- | The number of sessions running.
newtype Sessions = Sessions (TVar Int)

newSessions :: IO Sessions
newSessions = Sessions <$> newTVarIO 0

withSession :: Sessions -> IO a -> IO a
withSession (Sessions var) =
    E.bracket_ (atomically $ modifyTVar' var (+ 1)) (atomically $ modifyTVar' var (subtract 1))

waitSessions :: Sessions -> IO ()
waitSessions (Sessions var) = atomically $ readTVar var >>= check . (== 0)

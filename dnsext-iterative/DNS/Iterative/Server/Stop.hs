-- | Stopping a server by telling it, and waiting for the sessions it started.
module DNS.Iterative.Server.Stop (
    Stopper,
    newStopper,
    stopped,
    stopAll,
    stoppable,
    Sessions,
    newSessions,
    withSession,
    waitSessions,
) where

-- GHC packages
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception as E

-- | Telling servers to stop.  'stopAll' returns once every server has been
--   told; one which installs its action afterwards is told at once.
data Stopper = Stopper (TVar Bool) (MVar (Maybe [IO ()]))

newStopper :: IO Stopper
newStopper = Stopper <$> newTVarIO False <*> newMVar (Just [])

-- | Fires once 'stopAll' has been called.
stopped :: Stopper -> STM ()
stopped (Stopper var _) = readTVar var >>= check

stopAll :: Stopper -> IO ()
stopAll (Stopper var acts) = modifyMVar_ acts $ \m -> do
    atomically $ writeTVar var True
    sequence_ $ concat m
    return Nothing

-- | Running a server with an installer of the action which stops it.
stoppable :: Stopper -> ((IO () -> IO ()) -> IO a) -> IO a
stoppable (Stopper _ acts) server = server $ \act ->
    modifyMVar_ acts $ maybe (Nothing <$ act) (return . Just . (act :))

-- | The number of sessions running.
newtype Sessions = Sessions (TVar Int)

newSessions :: IO Sessions
newSessions = Sessions <$> newTVarIO 0

withSession :: Sessions -> IO a -> IO a
withSession (Sessions var) =
    E.bracket_ (atomically $ modifyTVar' var (+ 1)) (atomically $ modifyTVar' var (subtract 1))

waitSessions :: Sessions -> IO ()
waitSessions (Sessions var) = atomically $ readTVar var >>= check . (== 0)

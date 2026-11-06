-- | Stopping a server by telling it.
module DNS.Iterative.Server.Stop (
    Stopper,
    newStopper,
    stopped,
    stopAll,
    stoppable,
) where

-- GHC packages
import Control.Concurrent.MVar
import Control.Concurrent.STM

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

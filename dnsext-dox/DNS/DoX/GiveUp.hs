-- | What a DoX client gives up on, in STM, instead of being interrupted.
module DNS.DoX.GiveUp (
    deadline,
    GiveUp,
    newGiveUp,
    giveUpSTM,
    disarm,
    gaveUp,
    orTimeoutExpired,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (join)

import DNS.Types (DNSError (TimeoutExpired))

-- | Succeeding once the microseconds have passed.  A negative time is
--   never, as for 'System.Timeout.timeout'.
deadline :: Int -> IO (STM ())
deadline us
    | us < 0 = return retry
    | otherwise = (\late -> readTVar late >>= check) <$> registerDelay us

-- | What connecting gives up on, disarmed once the connection is up.
newtype GiveUp = GiveUp (TVar (STM ()))

newGiveUp :: STM () -> IO GiveUp
newGiveUp g = GiveUp <$> newTVarIO g

giveUpSTM :: GiveUp -> STM ()
giveUpSTM (GiveUp var) = join $ readTVar var

setGiveUp :: GiveUp -> STM () -> IO ()
setGiveUp (GiveUp var) = atomically . writeTVar var

-- | Giving up never from here.
disarm :: GiveUp -> IO ()
disarm gu = setGiveUp gu retry

-- | Whether it has succeeded.
gaveUp :: STM () -> IO Bool
gaveUp g = atomically $ (True <$ g) `orElse` return False

-- | Throwing 'TimeoutExpired' once the connection has been given up on,
--   as a timeout around it would have.
orTimeoutExpired :: Either () a -> IO a
orTimeoutExpired = either (\_ -> E.throwIO TimeoutExpired) return

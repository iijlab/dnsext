-- | What a DoX client gives up on, in STM, instead of being interrupted.
module DNS.DoX.GiveUp (
    deadline,
    GiveUp,
    newGiveUp,
    giveUpSTM,
    setGiveUp,
    disarm,
    withGiveUp,
    gaveUp,
    asTimeout,
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

-- | What a connection gives up on, pointed at one deadline after another.
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

-- | Pointing it at the STM for the action, and at 'retry' after.
withGiveUp :: GiveUp -> STM () -> IO a -> IO a
withGiveUp gu g action = (setGiveUp gu g >> action) `E.finally` disarm gu

-- | Whether it has succeeded.
gaveUp :: STM () -> IO Bool
gaveUp g = atomically $ (True <$ g) `orElse` return False

-- | Throwing 'TimeoutExpired' for whatever the action throws once the
--   'GiveUp' has given up, as a timeout around it would have.
asTimeout :: GiveUp -> IO a -> IO a
asTimeout gu action = action `E.catch` \e -> do
    given <- gaveUp $ giveUpSTM gu
    if given then E.throwIO TimeoutExpired else E.throwIO (e :: E.SomeException)

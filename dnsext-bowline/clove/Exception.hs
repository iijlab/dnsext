module Exception (
    trySync,
    loopLogErr,
    handleLogErr,
    logSomeErr,
) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E

import DNS.Auth.DB
import DNS.Log

import Types

-- | How long to wait after a failure before trying again.  Without it a
--   persistently failing action spins the CPU and floods the log.
retryDelay :: Int
retryDelay = 1000000

-- | 'E.try' restricted to synchronous exceptions.  Asynchronous ones,
--   such as the result of 'Control.Concurrent.killThread' or of a
--   timeout, are re-thrown: they are not ours to swallow.
trySync :: IO a -> IO (Either E.SomeException a)
trySync action = do
    ea <- E.try action
    case ea of
        Left se | isAsync se -> E.throwIO se
        _ -> return ea
  where
    isAsync se = case E.fromException se of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

logSomeErr :: Env -> Level -> E.SomeException -> IO ()
logSomeErr env lvl se = envPutLines env lvl Nothing [describe se]
  where
    describe e
        | Just (AuthException str) <- E.fromException e = str
        | otherwise = show e

-- | Repeating an action for ever.  Any synchronous exception is logged
--   and the action is tried again after a pause.  Catching 'IOError'
--   alone is not enough here: an 'AuthException' from the database or an
--   'E.ErrorCall' from a partial function would escape and take the
--   whole server down with it.
loopLogErr :: Env -> Level -> IO () -> IO ()
loopLogErr env lvl action = loop
  where
    loop = do
        ea <- trySync action
        case ea of
            Right () -> return ()
            Left se -> logSomeErr env lvl se >> threadDelay retryDelay
        loop

-- | Running an action, logging any synchronous exception and returning
--   the given value instead.
handleLogErr
    :: Env
    -> Level
    -> a
    -> IO a
    -> IO a
handleLogErr env lvl def body = do
    ea <- trySync body
    case ea of
        Right a -> return a
        Left se -> logSomeErr env lvl se >> return def

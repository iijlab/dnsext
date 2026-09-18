module Exception (
    trySync,
    loopLogErr,
    loopLogErrIn,
    handleLogErr,
    handleLogErrIn,
    logSomeErr,
    logSomeErrIn,
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
logSomeErr env lvl = logSomeErrIn env lvl ""

-- | Logging a failure with a note of what it was about in front of it.
--   A server holding several zones says little by reporting that some
--   file or other could not be read.
logSomeErrIn :: Env -> Level -> String -> E.SomeException -> IO ()
logSomeErrIn env lvl about se = envPutLines env lvl Nothing [about ++ describe se]
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
loopLogErr env lvl = loopLogErrIn env lvl ""

-- | 'loopLogErr' saying what the failures are about.
loopLogErrIn :: Env -> Level -> String -> IO () -> IO ()
loopLogErrIn env lvl about action = loop
  where
    loop = do
        ea <- trySync action
        case ea of
            Right () -> return ()
            Left se -> logSomeErrIn env lvl about se >> threadDelay retryDelay
        loop

-- | Running an action, logging any synchronous exception and returning
--   the given value instead.
handleLogErr
    :: Env
    -> Level
    -> a
    -> IO a
    -> IO a
handleLogErr env lvl = handleLogErrIn env lvl ""

-- | 'handleLogErr' saying what the failure was about.
handleLogErrIn
    :: Env
    -> Level
    -> String
    -> a
    -> IO a
    -> IO a
handleLogErrIn env lvl about def body = do
    ea <- trySync body
    case ea of
        Right a -> return a
        Left se -> logSomeErrIn env lvl about se >> return def

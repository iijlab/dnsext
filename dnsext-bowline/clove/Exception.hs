module Exception where

import qualified Control.Exception as E
import qualified System.IO.Error as E

import DNS.Auth.DB
import DNS.Log

import Types

handleIOError :: IO a -> (IOError -> IO a) -> IO a
handleIOError = E.catchIOError

logErr :: Env -> Level -> E.IOError -> IO ()
logErr env lvl ie = envPutLines env lvl Nothing [show ie]

loopLogErr :: Env -> Level -> IO () -> IO ()
loopLogErr env lvl action = loop
  where
    loop = do
        action `E.catchIOError` logErr env lvl
        loop

handleLogErr
    :: Env
    -> Level
    -> a
    -> IO a
    -> IO a
handleLogErr env lvl def body = body `E.catches` handlers
  where
    handlers =
        [ E.Handler $ \(AuthException str) ->
            envPutLines env lvl Nothing [str] >> return def
        , E.Handler $ \ie -> logErr env lvl ie >> return def
        ]

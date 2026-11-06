{-# LANGUAGE RecordWildCards #-}

module Types where

import Control.Concurrent.STM
import Control.Monad
import Data.ByteString.Builder
import Data.Functor
import Data.IORef
import System.IO.Error (tryIOError)

--

import DNS.Log (PutLines)
import DNS.RRCache (RRCacheOps)
import DNS.Types

import Config (Config)

{- FOURMOLU_DISABLE -}
data CacheControl = CacheControl
    { ccRemove          :: Domain -> IO ()
    , ccRemoveType      :: Domain -> TYPE -> IO ()
    , ccRemoveBogus     :: IO ()
    , ccRemoveNegative  :: IO ()
    , ccClear           :: IO ()
    }

data GlobalCache = GlobalCache
    { gcacheRRCacheOps  :: RRCacheOps
    , gcacheControl     :: CacheControl
    , gcacheSetLogLn    :: PutLines IO -> IO ()
    }
{- FOURMOLU_ENABLE -}

emptyCacheControl :: CacheControl
emptyCacheControl = CacheControl (\_ -> pure ()) (\_ _ -> pure ()) (pure ()) (pure ()) (pure ())

data ReloadCmd = Reload | KeepCache deriving (Show)

data QuitCmd = Quit | Reload1 Config | KeepCache1 Config deriving (Show)

data Control = Control
    { getStats :: IO Builder
    , getWStats :: IO Builder
    , reopenLog :: IO ()
    , getConfig :: IO (Either IOError Config)
    , quitServer :: IO ()
    , waitQuit :: STM ()
    , reloadSuccess :: ReloadCmd -> IO ()
    , reloadFailure :: ReloadCmd -> IO ()
    , getCommand :: IO QuitCmd
    , getCommandAndClear :: IO QuitCmd
    , setCommand :: QuitCmd -> IO ()
    }

newControl :: IO Config -> IO Control
newControl readConfig = do
    qRef <- newTVarIO False
    ref <- newIORef Quit
    return
        Control
            { getStats = return mempty
            , getWStats = return mempty
            , reopenLog = return ()
            , getConfig = tryIOError readConfig
            , quitServer = atomically $ writeTVar qRef True
            , waitQuit = readTVar qRef >>= guard
            , reloadSuccess = \_ -> pure ()
            , reloadFailure = \_ -> pure ()
            , getCommand = readIORef ref
            , getCommandAndClear = atomicModifyIORef' ref (\x -> (Quit, x))
            , setCommand = atomicWriteIORef ref
            }

quitCmd :: Control -> QuitCmd -> IO ()
quitCmd Control{..} cmd = setCommand cmd >> quitServer

reloadCmd :: Control -> ReloadCmd -> a -> a -> IO a
reloadCmd ctl@Control{..} rcmd lv rv = do
    either left right =<< getConfig
  where
    left e = putStrLn ("reload failed: " ++ show e) *> reloadFailure rcmd $> lv
    right conf = quitCmd ctl (cmd1 rcmd conf) *> reloadSuccess rcmd $> rv
    cmd1 Reload = Reload1
    cmd1 KeepCache = KeepCache1

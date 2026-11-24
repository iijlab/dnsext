{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module DNS.Log (
    newStdLogger,
    withStdLogger,
    newHandleLogger,
    withHandleLogger,
    Ops (..),
    LowOps (..),
    --
    Level (..),
    pattern DEMO,
    pattern WARN,
    pattern SYSTEM,
    --
    StdHandle (..),
    stdHandle,
    Logger,
    PutLines,
    StopLogger,
    ReopenLogger,
) where

-- GHC packages
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import Data.Functor
import Numeric.Natural
import System.IO (
    BufferMode (LineBuffering),
    Handle,
    hPutStrLn,
    hSetBuffering,
    stderr,
    stdout,
 )

-- other packages
import System.Console.ANSI (hSetSGR, hSupportsANSIColor)
import System.Console.ANSI.Types

-- this package

import qualified DNS.ThreadStats as TStat

{- FOURMOLU_DISABLE -}
data Level
    = DEBUG   -- ^ Most detailed.
    | INFO    -- ^ For demonstration
    | NOTICE  -- ^ (reserved for demo)
    | WARNING -- ^ Not error but caution is necessary (default)
    | ERR     -- ^ Fatal
    | CRIT    -- ^ (reserved)
    | ALERT   -- ^ (reserved)
    | EMERG   -- ^ (reserved)
    deriving (Eq, Ord, Show, Read)
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
{- levels for backword compat  -}
-- | Alias for 'INFO'
pattern DEMO     :: Level
pattern DEMO     = INFO
-- | Alias for 'WARNING'
pattern WARN     :: Level
pattern WARN     = WARNING
-- | Alias for 'ERR'
pattern SYSTEM   :: Level
pattern SYSTEM   = ERR
{- FOURMOLU_ENABLE -}

data StdHandle
    = Stdout
    | Stderr

instance Show StdHandle where
    show Stdout = "<stdout>"
    show Stderr = "<stderr>"

data Ops = Ops
    { putLinesSTM :: PutLines STM
    , putLines :: PutLines IO
    , reopenLogger :: IO ReopenLogger
    }

data LowOps = LowOps
    { runLogger :: IO Logger
    , stopLogger :: IO StopLogger
    }

type Logger = ()
type PutLines m = Level -> Maybe Color -> [String] -> m ()
type StopLogger = ()
type ReopenLogger = ()

-- | Creating 'Ops' for stdout or stderr.
newStdLogger :: StdHandle -> Level -> IO (Ops, LowOps)
newStdLogger oh lv = newHandleLogger (pure id) (pure $ stdHandle oh) (\_ -> pure ()) lv (\ops lowOps -> pure (ops, lowOps))

withStdLogger
    :: String
    -> StdHandle
    -> Level
    -> (Ops -> IO a)
    -> IO a
withStdLogger name oh lv body = do
    (ops, LowOps{..}) <- newStdLogger oh lv
    let run = void $ TStat.forkIO name runLogger
        stop = stopLogger
    E.bracket_ run stop $ body ops

{- FOURMOLU_DISABLE -}
-- | Creating logger based on 'Handle'.
newHandleLogger
    :: IO ShowS                -- ^ Getting log source
    -> IO Handle               -- ^ Open
    -> (Handle -> IO ())       -- ^ Close
    -> Level                   -- ^ Log level
    -> (Ops -> LowOps -> IO a) -- ^ Function which typically return 'Ops' and 'LowOps'
    -> IO a
newHandleLogger = makeHandleLogger queueBound
{- FOURMOLU_ENABLE -}

withHandleLogger
    :: String
    -- ^ Logger name
    -> IO ShowS
    -- ^ Getting log source
    -> IO Handle
    -- ^ Open
    -> (Handle -> IO ())
    -- ^ Close
    -> Level
    -- ^ Log level
    -> (Ops -> IO a)
    -> IO a
withHandleLogger name getM open close loggerLevel body = do
    (ops, LowOps{..}) <- makeHandleLogger queueBound getM open close loggerLevel $ \o lo -> pure (o, lo)
    let run = void $ TStat.forkIO name runLogger
        stop = stopLogger
    E.bracket_ run stop $ body ops

stdHandle :: StdHandle -> Handle
stdHandle Stdout = stdout
stdHandle Stderr = stderr

{- limit waiting area on server to constant size -}
queueBound :: Natural
queueBound = 8

{- FOURMOLU_DISABLE -}
makeHandleLogger
    :: Natural
    -> IO ShowS
    -> IO Handle
    -> (Handle -> IO ())
    -> Level
    -> (Ops -> LowOps -> IO a)
    -> IO a
makeHandleLogger qsize getM open close loggerLevel k = do
    outFh <- open'
    colorize <- hSupportsANSIColor outFh
    inQ <- newTBQueueIO qsize
    mvar <- newEmptyMVar
    let ops =
            Ops
                { putLinesSTM = putLinesSTM_ colorize inQ
                , putLines = putLinesIO_ colorize inQ
                , reopenLogger = reopen colorize inQ
                }
        lowOps =
            LowOps
                { runLogger = run inQ mvar outFh
                , stopLogger = stop inQ mvar
                }
    k ops lowOps
  where
    stop inQ mvar = do
        atomically $ writeTBQueue inQ $ \bk _ _ -> bk
        takeMVar mvar

    reopen colorize inQ = do
        putLinesIO_ colorize inQ INFO Nothing ["re-opening log."]
        atomically $ writeTBQueue inQ $ \_ rk _ -> rk

    putLinesSTM_ = putLines_ (pure id) id
    putLinesIO_ = putLines_ getM atomically

    putLines_ getM' toM colorize inQ lv ~color ~xs
        | colorize = withColor color
        | otherwise = withColor Nothing
      where
        withColor ~c = when (loggerLevel <= lv) $ do
            mdfy <- getM'
            toM $ writeTBQueue inQ $ \_ _ ck -> ck c (map mdfy xs)

    -- An IO error drops what it was for, and the logger stays to drain
    -- the queue: dying would block every 'putLines' and 'stopLogger'.
    run inQ mvar = loop . Just
      where
        loop mfh = do
            me <- atomically (readTBQueue inQ)
            let close' = mapM_ (trySync . close) mfh >> putMVar mvar ()
                reopen' = mapM_ (trySync . close) mfh >> trySync open' >>= loop
            me close' reopen' $ \c xs -> mapM_ (\fh -> trySync $ logit fh c xs) mfh >> loop mfh

    trySync :: IO b -> IO (Maybe b)
    trySync io = (Just <$> io) `E.catch` \e -> case E.fromException e of
        Just (E.SomeAsyncException _) -> E.throwIO e
        Nothing -> Nothing <$ E.try @E.SomeException (hPutStrLn stderr $ "logger: " ++ show e)

    open' = open >>= \outFh -> hSetBuffering outFh LineBuffering $> outFh

    logit outFh Nothing xs = mapM_ (hPutStrLn outFh) xs
    logit outFh (Just c) xs = do
        hSetSGR outFh [SetColor Foreground Vivid c]
        mapM_ (hPutStrLn outFh) xs
        hSetSGR outFh [Reset]
{- FOURMOLU_ENABLE -}

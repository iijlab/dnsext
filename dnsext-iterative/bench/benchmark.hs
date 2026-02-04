{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO, getNumCapabilities, killThread)
import Control.Concurrent.Async (concurrently_)
import Control.DeepSeq (deepseq)
import Control.Monad (replicateM, unless, (>=>))
import qualified DNS.Types as DNS
import qualified DNS.Types.Encode as DNS
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.UnixTime (diffUnixTime, getUnixTime)
import System.Console.GetOpt (
    ArgDescr (NoArg, ReqArg),
    ArgOrder (RequireOrder),
    OptDescr (Option),
    getOpt,
    usageInfo,
 )
import System.Environment (getArgs)
import System.Timeout (timeout)
import Text.Read (readEither)

import DNS.Iterative.Internal (Env (..), newEmptyEnv)
import DNS.Iterative.Server.Bench
import qualified DNS.Log as Log
import qualified DNS.RRCache as Cache
import DNS.TimeCache (getTime, newTimeCache)

data Config = Config
    { logOutput :: Log.StdHandle
    , logLevel :: Log.Level
    , maxCacheSize :: Int
    , noopMode :: Bool
    , gplotMode :: Bool
    , pipelines :: Int
    , qsizePerWorker :: Int
    , requests :: Int
    }

defaultOptions :: Config
defaultOptions =
    Config
        { logOutput = Log.Stdout
        , logLevel = Log.WARN
        , maxCacheSize = 2 * 1024
        , noopMode = False
        , gplotMode = False
        , pipelines = 2
        , qsizePerWorker = 16
        , requests = 512 * 1024
        }

descs :: [OptDescr (Config -> Either String Config)]
descs =
    [ Option
        ['h']
        ["help"]
        (NoArg $ const $ Left "show help")
        "show this help text"
    , -- , Option [] ["log-output"]
      --   (ReqArg (\s opts -> parseOutput s >>= \x -> return opts { logOutput = x }) $ "{" ++ intercalate "|" (map fst outputs) ++ "}")
      --   "log output target. default is stdout"
      -- , Option ['l'] ["log-level"]
      --   (ReqArg (\s opts -> readEither (map toUpper s) >>= \x -> return opts { logLevel = x }) "{WARN|NOTICE|INFO|DEBUG}")
      --   "server log-level"
      -- , Option ['M'] ["max-cache-entries"]
      --   (ReqArg (\s opts -> readIntWith (> 0) "max-cache-entries. not positive size" s >>= \x -> return opts { maxKibiEntries = x }) "POSITIVE_INTEGER")
      --   ("max K-entries in cache (1024 entries per 1). default is " ++ show (maxKibiEntries defaultOptions) ++ " K-entries")
      Option
        []
        ["noop"]
        (NoArg $ \opts -> return opts{noopMode = True})
        "No-op mode"
    , Option
        []
        ["plot"]
        (NoArg $ \opts -> return opts{gplotMode = True})
        "output for GNUplot"
    , Option
        ['p']
        ["pipelines"]
        ( ReqArg
            ( \s opts ->
                readIntWith (> 0) "pipelines. not positive" s >>= \x -> return opts{pipelines = x}
            )
            "POSITIVE_INTEGER"
        )
        "workers per host. default is 2"
    , -- , Option [] ["no-shared-queue"]
      --   (NoArg $ \opts -> return opts { workerSharedQueue = False })
      --   "not share request queue and response queue in worker threads"
      -- , Option
      --   []
      --   ["per-worker"]
      --   ( ReqArg
      --       ( \s opts ->
      --           readIntWith (>= 0) "per-worker. negative not allowed" s >>= \x -> return opts{qsizePerWorker = x}
      --       )
      --       "POSITIVE_INTEGER"
      --   )
      --   "queue size per worker. default is 16. positive integer or 0. 0 means not limited size queue"
      Option
        ['r']
        ["requests"]
        ( ReqArg
            ( \s opts ->
                readIntWith (> 0) "requests. not positive" s >>= \x -> return opts{requests = x}
            )
            "POSITIVE_INTEGER"
        )
        "requests count. default is 512 * 1024"
    ]
  where
    readIntWith p em s = do
        x <- readEither s :: Either String Int
        unless (p x) $ Left $ em ++ ": " ++ show x
        return $ fromIntegral x

-- parseOutput s = maybe (Left "unknown log output target") Right $ lookup s outputs
-- outputs = [("stdout", Log.Stdout), ("stderr", Log.Stderr)]

help :: IO ()
help =
    putStr $
        usageInfo
            "benchmark [options]"
            descs

parseOptions :: [String] -> IO (Maybe Config)
parseOptions args
    | not (null errs) = mapM putStrLn errs *> return Nothing
    | otherwise =
        either helpOnLeft (return . Just) $
            foldr (>=>) return ars defaultOptions
  where
    (ars, _rest, errs) = getOpt RequireOrder descs args
    helpOnLeft e = putStrLn e *> help *> return Nothing

run :: Config -> IO ()
run conf@Config{..} = runBenchmark conf noopMode gplotMode requests

main :: IO ()
main = maybe (return ()) run =<< parseOptions =<< getArgs

runBenchmark
    :: Config
    -> Bool
    -- ^ No operation or not
    -> Bool
    -- ^ Gnuplot mode or not
    -> Int
    -- ^ Request size
    -> IO ()
runBenchmark conf@Config{..} noop gplot size = do
    (logger, _, putLines, flush) <- Log.new logOutput logLevel
    tid <- forkIO logger
    env <- getEnv conf putLines

    (workers, enqueueReq, dequeueResp) <- benchServer pipelines env noop
    _ <- forkIO $ foldr concurrently_ (return ()) workers

    let (initD, ds) = splitAt 4 $ take (4 + size) benchQueries
    ds `deepseq` return ()

    -----
    _ <- runQueries initD enqueueReq dequeueResp
    before <- getUnixTime
    _ <- runQueries ds enqueueReq dequeueResp
    after <- getUnixTime

    let elapsed = toRational $ after `diffUnixTime` before
        toDouble = fromRational :: Rational -> Double
        rate = fromIntegral size / elapsed

        pipelines_per_socket = pipelines

    if gplot
        then do
            putStrLn $ unwords [show pipelines_per_socket, show rate]
        else do
            putStrLn . ("capabilities: " ++) . show =<< getNumCapabilities
            putStrLn $ "pipelines: " ++ show pipelines_per_socket
            -- putStrLn $ "qsizePerPipeline: " ++ show udp_queue_size_per_pipeline
            putStrLn . ("cache size: " ++) . show . Cache.size =<< getCache_ env
            putStrLn $ "requests: " ++ show size
            putStrLn $ "elapsed: " ++ show (toDouble elapsed) ++ " (sec)"
            putStrLn $ "rate: " ++ show (toDouble rate)
    killThread tid
    flush

{- FOURMOLU_DISABLE -}
getEnv :: Config -> Log.PutLines IO -> IO Env
getEnv Config{..} putLines = do
    tcache <- newTimeCache
    let memoLogLn = putLines Log.WARN Nothing . (: [])
        cacheConf = Cache.RRCacheConf maxCacheSize 1800 memoLogLn $ getTime tcache
    Cache.RRCacheOps{..} <- Cache.newRRCacheOps cacheConf
    env <- newEmptyEnv
    pure env{ disableV6NS_ = False, logLines_ = putLines, currentSeconds_ = getTime tcache
            , insert_ = insertCache, getCache_ = readCache, expireCache_ = expireCache, timeout_ = timeout 3000000}
{- FOURMOLU_ENABLE -}

runQueries :: [a1] -> ((a1, ()) -> IO a2) -> IO a3 -> IO [a3]
runQueries qs enqueueReq dequeueResp = do
    _ <- forkIO $ sequence_ [enqueueReq (q, ()) | q <- qs]
    replicateM len dequeueResp
  where
    len = length qs

benchQueries :: [ByteString]
benchQueries =
    [ DNS.encode $ setId mid rootA {- TODO: seq ByteString ? -}
    | mid <- cycle [0 .. maxBound]
    | rootA <- cycle rootAs
    ]
  where
    setId mid qm = qm{DNS.identifier = mid}
    rootAs =
        [ DNS.defaultQuery
            { DNS.question = DNS.Question (fromString name) DNS.A DNS.IN
            }
        | c1 <- ["a", "b", "c", "d"]
        , let name = c1 ++ ".root-servers.net."
        ]

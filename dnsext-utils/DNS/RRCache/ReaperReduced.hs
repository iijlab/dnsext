{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.RRCache.ReaperReduced (
    -- * Example: Regularly cleaning a cache
    -- $example1

    -- * Settings
    ReaperSettings,
    defaultReaperSettings,

    -- * Accessors
    reaperAction,
    reaperCallback,
    reaperDelay,
    reaperNull,
    reaperEmpty,

    -- * Type
    Reaper (..),

    -- * Creation
    mkReaper,

    -- * Helper
    mkListAction,
)
where

import Control.Concurrent (ThreadId, killThread, threadDelay)
import qualified Control.Exception as E
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

import qualified DNS.ThreadStats as TStat

data ReaperSettings workload = ReaperSettings
    { reaperAction :: IO (workload -> Maybe workload)
    , reaperCallback :: Maybe workload -> IO ()
    , reaperDelay :: Int
    , reaperNull :: workload -> Bool
    , reaperEmpty :: workload
    }

-- | Default @ReaperSettings@ value, biased towards having a list of work
-- items.
defaultReaperSettings :: ReaperSettings [item]
defaultReaperSettings =
    ReaperSettings
        { reaperAction = return Just
        , reaperCallback = const $ return ()
        , reaperDelay = 30000000
        , reaperNull = null
        , reaperEmpty = []
        }

-- | A data structure to hold reaper APIs.
data Reaper workload = Reaper
    { reaperUpdate :: (workload -> workload) -> IO ()
    -- ^ Updating the workload. require function to update
    , reaperRead :: IO workload
    -- ^ Reading workload.
    , reaperStop :: IO workload
    -- ^ Stopping the reaper thread if exists.
    --   The current workload is returned.
    , reaperKill :: IO ()
    -- ^ Killing the reaper thread immediately if exists.
    }

-- | State of reaper.
data State workload
    = -- | No reaper thread
      NoReaper
    | -- | The current jobs
      Workload workload

-- | Create a reaper addition function. This function can be used to add
-- new items to the workload. Spawning of reaper threads will be handled
-- for you automatically.
mkReaper :: ReaperSettings workload -> IO (Reaper workload)
mkReaper settings@ReaperSettings{..} = do
    stateRef <- newIORef NoReaper
    lookupRef <- newIORef NoReaper {- only allowed after reduced thunk pointer -}
    tidRef <- newIORef Nothing
    return
        Reaper
            { reaperUpdate = update settings stateRef lookupRef tidRef
            , reaperRead = readRef lookupRef
            , reaperStop = stop stateRef
            , reaperKill = kill tidRef
            }
  where
    readRef lookupRef = do
        mx <- readIORef lookupRef
        case mx of
            NoReaper -> return reaperEmpty
            Workload wl -> return wl
    stop stateRef = atomicModifyIORef' stateRef $ \case
        NoReaper -> (NoReaper, reaperEmpty)
        Workload x -> (Workload reaperEmpty, x)
    kill tidRef = do
        mtid <- readIORef tidRef
        case mtid of
            Nothing -> return ()
            Just tid -> killThread tid

update
    :: ReaperSettings workload
    -> IORef (State workload)
    -> IORef (State workload)
    -> IORef (Maybe ThreadId)
    -> (workload -> workload)
    -> IO ()
update settings@ReaperSettings{..} stateRef lookupRef tidRef modifyWL =
    E.mask_ $ do
        next <- atomicModifyIORef' stateRef modify
        next
  where
    modify NoReaper =
        let thunk = Workload (modifyWL reaperEmpty)
         in (thunk, writeIORef lookupRef thunk *> spawn settings stateRef lookupRef tidRef)
    modify (Workload wl) =
        let thunk = Workload (modifyWL wl)
         in (thunk, writeIORef lookupRef thunk)

spawn
    :: ReaperSettings workload
    -> IORef (State workload)
    -> IORef (State workload)
    -> IORef (Maybe ThreadId)
    -> IO ()
spawn settings stateRef lookupRef tidRef = do
    tid <- TStat.forkIO "bw.rrcache-reaper" $ reaper settings stateRef lookupRef tidRef
    writeIORef tidRef $ Just tid

reaper
    :: ReaperSettings workload
    -> IORef (State workload)
    -> IORef (State workload)
    -> IORef (Maybe ThreadId)
    -> IO ()
reaper settings@ReaperSettings{..} stateRef lookupRef tidRef = do
    threadDelay reaperDelay
    prune <- reaperAction
    next <- atomicModifyIORef' stateRef (checkPrune prune)
    next
  where
    checkPrune _ NoReaper = error "Control.Reaper.reaper: unexpected NoReaper (1)"
    checkPrune prune current@(Workload wl) = case mayWl' of
        Nothing ->
            ( current
            , do
                callback
                reaper settings stateRef lookupRef tidRef
            )
        Just wl'
            -- If there is no job, reaper is terminated.
            | reaperNull wl' ->
                ( NoReaper
                , do
                    callback
                    writeIORef lookupRef NoReaper
                    writeIORef tidRef Nothing
                )
            -- If there are jobs, carry them out.
            | otherwise ->
                let thunk = Workload wl'
                 in ( thunk
                    , do
                        callback
                        writeIORef lookupRef thunk
                        reaper settings stateRef lookupRef tidRef
                    )
      where
        mayWl' = prune wl
        callback = reaperCallback mayWl'

-- | A helper function for creating 'reaperAction' functions. You would
-- provide this function with a function to process a single work item and
-- return either a new work item, or @Nothing@ if the work item is
-- expired.
mkListAction
    :: (item -> IO (Maybe item'))
    -> [item]
    -> IO ([item'] -> [item'])
mkListAction f =
    go id
  where
    go front [] = return front
    go front (x : xs) = do
        my <- f x
        let front' =
                case my of
                    Nothing -> front
                    Just y -> front . (y :)
        go front' xs

-- $example1
-- Example of caching fibonacci numbers like Reaper
--
-- -- @
--
-- import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
-- import DNS.ReaperReduced
-- import Control.Concurrent (threadDelay)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Control.Monad (forever)
-- import System.IO (BufferMode (..), hSetBuffering, stdout)
-- import System.Random (getStdRandom, randomR)
--
-- fib :: Int -> Int
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
--
-- type Cache = Map Int (Int, UTCTime)
--
-- main :: IO ()
-- main = do
--   hSetBuffering stdout LineBuffering
--   reaper <- mkReaper defaultReaperSettings
--     { reaperAction = clean
--     , reaperCallback =
--         \x -> case x of
--                 Just m | Map.null m ->  putStrLn "clean: empty"
--                 _                   ->  putStrLn "clean: not empty"
--     , reaperDelay = 1000000 * 2 -- Clean 2 seconds after
--     , reaperNull = Map.null
--     , reaperEmpty = Map.empty
--     }
--   forever $ do
--     fibArg <- System.Random.getStdRandom (System.Random.randomR (30,34))
--     cache <- reaperRead reaper
--     let cachedResult = Map.lookup fibArg cache
--     case cachedResult of
--       Just (fibResult, _createdAt) -> putStrLn $ "Found in cache: `fib " ++ show fibArg ++ "` " ++ show fibResult
--       Nothing -> do
--         let fibResult = fib fibArg
--         putStrLn $ "Calculating `fib " ++ show fibArg ++ "` " ++ show fibResult
--         time <- getCurrentTime
--         reaperUpdate reaper $ Map.insert fibArg (fibResult, time)
--     threadDelay 1000000 -- 1 second
--
-- -- Remove items > 10 seconds old
-- clean :: IO (Cache -> Maybe Cache)
-- clean = do
--   currentTime <- getCurrentTime
--   let prune oldMap = Just (Map.filter (\ (_, createdAt) -> currentTime `diffUTCTime` createdAt < 10.0) oldMap)
--   return prune
--
-- -- @

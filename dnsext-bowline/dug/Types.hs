{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified DNS.Log as Log
import DNS.Transport.Types (Synthesis (..))
import Data.ByteString.Short (ShortByteString)

import Output (OutputFlag (..))

data Options = Options
    { optHelp :: Bool
    , optIterative :: Bool
    , optDisableV6NS :: Bool
    , optPort :: Maybe String
    , optRootHints :: Maybe FilePath
    , optTrustAnchor :: Maybe FilePath
    , optDoX :: ShortByteString
    , optSynthesis :: Synthesis
    , optFormat :: OutputFlag
    , optVerboseLevel :: Int
    , optKeyLogFile :: Maybe FilePath
    , optResumptionFile :: Maybe FilePath
    , opt0RTT :: Bool
    , optValidate :: Bool
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { optHelp = False
        , optIterative = False
        , optDisableV6NS = False
        , optPort = Nothing
        , optRootHints = Nothing
        , optTrustAnchor = Nothing
        , optDoX = "do53"
        , optSynthesis = SynthNone
        , optFormat = Singleline
        , optVerboseLevel = 0
        , optKeyLogFile = Nothing
        , optResumptionFile = Nothing
        , opt0RTT = False
        , optValidate = False
        }

shortLog :: Options -> Bool
shortLog opt = optVerboseLevel opt == 1

{- FOURMOLU_DISABLE -}
logLevel :: Options -> Log.Level
logLevel opt
    | verbose <= 0  = Log.WARN
    | verbose == 1  = Log.DEMO  {- for short-log mode with DEMO log-level -}
    | verbose == 2  = Log.DEMO
    | otherwise     = Log.DEBUG
  where
    verbose = optVerboseLevel opt
{- FOURMOLU_ENABLE -}

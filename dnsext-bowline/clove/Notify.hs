{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Notify where

import Data.IP
import Data.List.NonEmpty ()

import qualified System.IO.Error as E

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.Types

import Exception
import Types

notify :: Env -> Domain -> IP -> IO (Maybe DNSMessage)
notify Env{..} dom ip = withNotified $ do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left e -> do
            envPutLines
                WARNING
                Nothing
                ["    NOTIFY @" ++ show ip ++ " \"" ++ toRepresentation dom ++ "\": " ++ show e]
            return Nothing
        Right msg -> return $ Just msg
  where
    riActions =
        defaultResolveActions
            { ractionTimeoutTime = 3000000
            , ractionLog = envPutLines
            }
    ris =
        [ defaultResolveInfo
            { rinfoIP = ip
            , rinfoPort = 53
            , rinfoActions = riActions
            , rinfoUDPRetry = 3
            , rinfoVCLimit = 0
            }
        ]
    renv =
        ResolveEnv
            { renvResolver = udpResolver
            , renvConcurrent = True -- should set True if multiple RIs are provided
            , renvResolveInfos = ris
            }
    q = Question dom SOA IN
    -- RFC 5936: DNS Zone Transfer Protocol (AXFR)
    qctl = rdFlag FlagClear <> doFlag FlagClear <> aaFlag FlagSet <> opCode OP_NOTIFY
    -- Saying which zone and which secondary a failure belongs to.
    withNotified action = do
        er <- trySync action
        case er of
            Right a -> return a
            Left se ->
                E.ioError $
                    E.userError $
                        "NOTIFY @" ++ show ip ++ " \"" ++ toRepresentation dom ++ "\": " ++ show se

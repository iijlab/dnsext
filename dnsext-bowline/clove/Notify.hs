{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Notify where

import Data.IP
import Data.List.NonEmpty ()
import Network.Socket (PortNumber)

import qualified System.IO.Error as E

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.Types

import Exception
import Types

notify :: Env -> Domain -> IP -> PortNumber -> IO (Maybe DNSMessage)
notify Env{..} dom ip port = withNotified $ do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left e -> do
            envPutLines
                WARNING
                Nothing
                ["    NOTIFY " ++ peer ++ ": " ++ show e]
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
            , rinfoPort = port
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
    peer = "@" ++ show ip ++ "#" ++ show port ++ " \"" ++ toRepresentation dom ++ "\""
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
                        "NOTIFY " ++ peer ++ ": " ++ show se

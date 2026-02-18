{-# LANGUAGE OverloadedLists #-}

module Notify where

import Data.IP
import Data.List.NonEmpty ()

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Types

notify :: Domain -> IP -> IO (Maybe DNSMessage)
notify dom ip = do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left _ -> return Nothing
        Right msg -> return $ Just msg
  where
    riActions =
        defaultResolveActions
            { ractionTimeoutTime = 3000000
            , ractionLog = \_lvl _mclr ss -> mapM_ putStrLn ss
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
    -- fixme: AA flags
    q = Question dom SOA IN
    qctl = rdFlag FlagClear <> doFlag FlagClear <> aaFlag FlagSet <> opCode OP_NOTIFY

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Axfr (
    transfer,
    tcpAllowAXFR,
    client,
) where

import Data.IORef
import Data.IP
import qualified Data.IP.RouteTable as T
import Data.List as List
import Data.List.NonEmpty ()
import Data.Maybe
import Network.Socket

import DNS.Auth.Algorithm
import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.Types
import DNS.Types.Encode

import Types

tcpAllowAXFR :: SockAddr -> Domain -> ZoneAlist -> IO (Maybe Zone)
tcpAllowAXFR sa dom zoneAlist = case List.lookup dom zoneAlist of -- exact match
    Nothing -> return Nothing
    Just zoneref -> do
        zone <- readIORef zoneref
        if accessControl zone
            then return $ Just zone
            else return Nothing
  where
    accessControl zone = case fromSockAddr sa of
        Just (IPv4 ip4, _) -> fromMaybe False $ T.lookup (makeAddrRange ip4 32) t4
        Just (IPv6 ip6, _) -> fromMaybe False $ T.lookup (makeAddrRange ip6 128) t6
        _ -> False
      where
        t4 = zoneAllowTransfer4 zone
        t6 = zoneAllowTransfer6 zone

transfer :: Env -> Proto -> Zone -> SockAddr -> DNSMessage -> IO ()
transfer Env{..} Proto{..} zone sa query = do
    let db = zoneDB zone
        reply = (fromQuery query){answer = dbAll db}
        (ip, port) = fromJust $ fromSockAddr sa
    envPutLines
        NOTICE
        Nothing
        ["    axfr @" ++ show ip ++ "#" ++ show port ++ "/TCP \"" ++ toRepresentation (zoneName zone) ++ "\""]
    sendReply sa $ encode reply

----------------------------------------------------------------

client :: Env -> Maybe Serial -> IP -> Domain -> IO [ResourceRecord]
client env Nothing ip dom = axfrQuery env ip dom
client env (Just serial0) ip dom = do
    mserial <- serialQuery env ip dom
    case mserial of
        Nothing -> return []
        Just serial
            | serial > serial0 -> axfrQuery env ip dom
            | otherwise -> return []

serialQuery :: Env -> IP -> Domain -> IO (Maybe Serial)
serialQuery Env{..} ip dom = do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left _ -> return Nothing
        Right msg -> case answer msg of
            [] -> return Nothing
            soa : _ -> case fromRData $ rdata soa of
                Nothing -> return Nothing
                Just s -> return $ Just $ soa_serial s
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
    qctl = rdFlag FlagClear <> doFlag FlagClear

axfrQuery :: Env -> IP -> Domain -> IO [ResourceRecord]
axfrQuery Env{..} ip dom = do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left _ -> return []
        Right msg -> return $ checkSOA $ answer msg
  where
    riActions =
        defaultResolveActions
            { ractionTimeoutTime = 30000000
            , ractionLog = envPutLines
            }
    ris =
        [ defaultResolveInfo
            { rinfoIP = ip
            , rinfoPort = 53
            , rinfoActions = riActions
            , rinfoUDPRetry = 1
            , rinfoVCLimit = 32 * 1024
            }
        ]
    renv =
        ResolveEnv
            { renvResolver = tcpResolver
            , renvConcurrent = True -- should set True if multiple RIs are provided
            , renvResolveInfos = ris
            }
    q = Question dom AXFR IN
    qctl = rdFlag FlagClear <> doFlag FlagClear

checkSOA :: [ResourceRecord] -> [ResourceRecord]
checkSOA [] = []
checkSOA (soa : rrs)
    | rrtype soa == SOA =
        case unsnoc' rrs of
            Nothing -> []
            Just (rrs', soa')
                | rrtype soa' == SOA -> soa : rrs'
                | otherwise -> []
    | otherwise = []
  where
    unsnoc' = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

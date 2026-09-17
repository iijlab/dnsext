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
import qualified System.IO.Error as E

import DNS.Auth.Algorithm
import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.Types
import DNS.Types.Encode

import Exception
import Types

-- | Saying which zone and which upstream a failure belongs to.  Without
--   it the operator is told only that some socket somewhere would not
--   connect.
withUpstream :: IP -> Domain -> String -> IO a -> IO a
withUpstream ip dom what action = do
    er <- trySync action
    case er of
        Right a -> return a
        Left se ->
            E.ioError $
                E.userError $
                    what ++ " @" ++ show ip ++ " \"" ++ toRepresentation dom ++ "\": " ++ show se

-- | Saying that we asked and did not get a usable answer.  'Nothing'
--   rather than an error: failing to reach the upstream this once is
--   something to carry on from, not to stop for.
unanswered :: Env -> IP -> Domain -> String -> String -> IO (Maybe a)
unanswered Env{..} ip dom what why = do
    envPutLines
        WARNING
        Nothing
        ["    " ++ what ++ " @" ++ show ip ++ " \"" ++ toRepresentation dom ++ "\": " ++ why]
    return Nothing

tcpAllowAXFR :: SockAddr -> Domain -> ZoneAlist -> IO (Maybe Zone)
tcpAllowAXFR sa dom zoneAlist = case List.lookup dom zoneAlist of -- exact match
    Nothing -> return Nothing
    Just zoneref -> do
        zone <- readIORef zoneref
        -- Transferring a zone which is not loaded would hand out the
        -- empty database, that is a zero record AXFR response.
        if zoneReady zone && accessControl zone
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
        peer = maybe (show sa) (\(ip, port) -> show ip ++ "#" ++ show port) $ fromSockAddr sa
    envPutLines
        NOTICE
        Nothing
        ["    axfr @" ++ peer ++ "/TCP \"" ++ toRepresentation (zoneName zone) ++ "\""]
    sendReply sa $ encode reply

----------------------------------------------------------------

-- | Transferring the zone when the upstream has something newer than
--   the serial given.  'Nothing' means there is nothing to transfer --
--   because the upstream has not moved on, or because it could not be
--   asked.  It does not mean the zone is empty, and it is not an error:
--   a failing transfer throws instead.
client :: Env -> Maybe Serial -> IP -> Domain -> IO (Maybe [ResourceRecord])
client env Nothing ip dom = Just <$> axfrQuery env ip dom
client env (Just serial0) ip dom = do
    mserial <- serialQuery env ip dom
    case mserial of
        Nothing -> return Nothing
        Just serial
            | serial > serial0 -> Just <$> axfrQuery env ip dom
            | otherwise -> return Nothing

serialQuery :: Env -> IP -> Domain -> IO (Maybe Serial)
serialQuery env@Env{..} ip dom = withUpstream ip dom "SOA" $ do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left e -> unanswered env ip dom "SOA" $ show e
        Right msg -> case answer msg of
            [] -> unanswered env ip dom "SOA" "no SOA in the answer"
            soa : _ -> case fromRData $ rdata soa of
                Nothing -> unanswered env ip dom "SOA" "broken SOA"
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
axfrQuery Env{..} ip dom = withUpstream ip dom "AXFR" $ do
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

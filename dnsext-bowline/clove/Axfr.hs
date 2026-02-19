{-# LANGUAGE OverloadedLists #-}

module Axfr (
    server,
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
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Types

----------------------------------------------------------------

server
    :: ZoneAlist
    -> Socket
    -> IO ()
server zoneAlist sock = do
    sa <- getPeerName sock
    equery <- decode <$> recvVC (32 * 1024) (recvTCP sock)
    case equery of
        Left _ -> return ()
        Right query -> do
            let dom = qname $ question query
            case List.lookup dom zoneAlist of -- exact match
                Nothing -> replyRefused query
                Just zoneref -> do
                    zone <- readIORef zoneref
                    if accessControl zone sa
                        then do
                            let db = zoneDB zone
                            let reply = makeReply db query
                            sendVC (sendTCP sock) $ encode reply
                        else replyRefused query
  where
    replyRefused query = sendVC (sendTCP sock) $ encode reply
      where
        reply = (fromQuery query){rcode = Refused}
    accessControl zone sa = case fromSockAddr sa of
        Just (IPv4 ip4, _) -> fromMaybe False $ T.lookup (makeAddrRange ip4 32) t4
        Just (IPv6 ip6, _) -> fromMaybe False $ T.lookup (makeAddrRange ip6 128) t6
        _ -> False
      where
        t4 = zoneAllowTransfer4 zone
        t6 = zoneAllowTransfer6 zone

makeReply :: DB -> DNSMessage -> DNSMessage
makeReply db query
    | qtype (question query) == AXFR = (fromQuery query){answer = dbAll db}
    | otherwise = getAnswer db query

----------------------------------------------------------------

client :: Serial -> IP -> Domain -> IO [ResourceRecord]
client serial0 ip dom = do
    mserial <- serialQuery ip dom
    case mserial of
        Nothing -> return []
        Just serial
            | serial /= serial0 -> axfrQuery ip dom
            | otherwise -> return []

serialQuery :: IP -> Domain -> IO (Maybe Serial)
serialQuery ip dom = do
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
    q = Question dom SOA IN
    qctl = rdFlag FlagClear <> doFlag FlagClear

axfrQuery :: IP -> Domain -> IO [ResourceRecord]
axfrQuery ip dom = do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left _ -> return []
        Right msg -> return $ checkSOA $ answer msg
  where
    riActions =
        defaultResolveActions
            { ractionTimeoutTime = 30000000
            , ractionLog = \_lvl _mclr ss -> mapM_ putStrLn ss
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
        case unsnoc rrs of
            Nothing -> []
            Just (rrs', soa')
                | rrtype soa' == SOA -> soa : rrs'
                | otherwise -> []
    | otherwise = []

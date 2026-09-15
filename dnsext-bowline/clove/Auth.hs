{-# LANGUAGE RecordWildCards #-}

module Auth (server, tcpAllowAXFR) where

import DNS.Auth.Algorithm
import DNS.Log
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Data.ByteString (ByteString)
import Data.IORef
import Data.IP
import Data.Maybe
import Network.Socket

import Axfr
import Exception
import Types
import Zone

server :: Env -> Proto -> ZoneAlist -> IO ()
server env@Env{..} proto@Proto{..} zoneAlist = loopLogErr env DEBUG go
  where
    go = do
        (bs, sa) <- recvQuery
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> case opcode query of
                OP_NOTIFY -> handleNotify proto zoneAlist sa query
                OP_STD -> do
                    let q = question query
                        dom = qname q
                        typ = qtype q
                        (ip, _port) = fromJust $ fromSockAddr sa
                    envPutLines
                        DEBUG
                        Nothing
                        ["\"" ++ toRepresentation dom ++ "\" " ++ show typ ++ " from " ++ show ip ++ "/" ++ protoName]
                    if typ == AXFR || typ == IXFR
                        then do
                            -- RFC 1995 Sec 4
                            -- If incremental zone transfer is not
                            -- available, the entire zone is returned.
                            -- The first and the last RR of the response
                            -- is the SOA record of the zone.  I.e. the
                            -- behavior is the same as an AXFR response
                            -- except the query type is IXFR.
                            mx <- allowAXFR sa dom zoneAlist
                            case mx of
                                Nothing -> sendReply sa $ replyRefused query
                                Just zone -> transfer env proto zone sa query
                        else
                            response proto zoneAlist sa query dom
                _ -> sendReply sa $ replyRefused query

response :: Proto -> ZoneAlist -> SockAddr -> DNSMessage -> Domain -> IO ()
response Proto{..} zoneAlist sa query dom = case findZoneAlist dom zoneAlist of -- isSubDomainOf
    Nothing -> sendReply sa $ replyRefused query
    Just (_, zoneref) -> do
        zone <- readIORef zoneref
        sendReply sa $ replyQuery query $ zoneDB zone

handleNotify :: Proto -> ZoneAlist -> SockAddr -> DNSMessage -> IO ()
handleNotify Proto{..} zoneAlist sa query = case lookup dom zoneAlist of -- exact match
    Nothing -> sendReply sa $ replyRefused query
    Just zoneref -> do
        Zone{..} <- readIORef zoneref
        case fromSockAddr sa of
            Nothing -> sendReply sa $ replyRefused query
            Just (ip, _)
                | ip `elem` zoneAllowNotifyAddrs -> do
                    sendReply sa $ replyNotice query
                    zoneWakeUp
                | otherwise -> sendReply sa $ replyRefused query
  where
    dom = qname $ question query

replyNotice :: DNSMessage -> ByteString
replyNotice query = encode $ fromQuery query

replyQuery :: DNSMessage -> DB -> ByteString
replyQuery query db = encode $ getAnswer db query

replyRefused :: DNSMessage -> ByteString
replyRefused query = encode $ (fromQuery query){rcode = Refused}

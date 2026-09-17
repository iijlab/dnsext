{-# LANGUAGE RecordWildCards #-}

module Auth (server, tcpAllowAXFR) where

import DNS.Auth.Algorithm
import DNS.Log
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import Data.IORef
import Data.IP
import Network.Socket

import Axfr
import Exception
import Types
import Zone

-- | How many receive errors in a row are tolerated before the server
--   starts to back off.
recvErrorBurst :: Int
recvErrorBurst = 10

-- | How long to pause once a socket keeps failing.
recvRetryDelay :: Int
recvRetryDelay = 1000000

server :: Env -> Proto -> ZoneAlist -> IO ()
server env@Env{..} proto@Proto{..} zoneAlist = loop 0
  where
    loop nerr = do
        er <- trySync recvQuery
        case er of
            Left se
                -- Over a connection this means the peer is gone.  Leave
                -- the loop; retrying would spin on a dead descriptor.
                | recvErrorFatal -> logSomeErr env DEBUG se
                -- A datagram socket, on the other hand, survives an
                -- error.  Linux even reports an asynchronous error for
                -- an earlier reply of ours through a later receive
                -- (udp(7)), which any client can provoke by closing its
                -- socket, so a single error must not stop the server.
                -- Only a socket which keeps failing without delivering
                -- anything is worth a warning and a pause.
                | nerr < recvErrorBurst -> do
                    logSomeErr env DEBUG se
                    loop (nerr + 1)
                | otherwise -> do
                    logSomeErr env WARNING se
                    threadDelay recvRetryDelay
                    loop (nerr + 1)
            Right query -> do
                -- Failing to answer one query must never take the
                -- server down either.
                handleLogErr env WARNING () $ go query
                loop 0
    go (bs, sa) =
        case decode bs of
            -- fixme: which RFC?
            Left _e -> return ()
            Right query -> case opcode query of
                OP_NOTIFY -> handleNotify proto zoneAlist sa query
                OP_STD -> do
                    let q = question query
                        dom = qname q
                        typ = qtype q
                        peer = maybe (show sa) (show . fst) $ fromSockAddr sa
                    envPutLines
                        DEBUG
                        Nothing
                        ["\"" ++ toRepresentation dom ++ "\" " ++ show typ ++ " from " ++ peer ++ "/" ++ protoName]
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
        -- A zone whose source could not be loaded holds the empty
        -- database, whose apex is the root, so every name in it would be
        -- answered with an authoritative NXDOMAIN and the placeholder
        -- SOA of that database.  We would be denying the existence of
        -- names we simply know nothing about, and downstream caches
        -- would keep the denial.
        if zoneReady zone
            then sendReply sa $ replyQuery query $ zoneDB zone
            else sendReply sa $ replyServFail query

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

-- | We are configured for this zone but have nothing to say about it.
--   Not authoritative: there is no data to be authoritative about.
replyServFail :: DNSMessage -> ByteString
replyServFail query = encode reply{rcode = ServFail, flags = flgs}
  where
    reply = fromQuery query
    flgs = (flags reply){authAnswer = False}

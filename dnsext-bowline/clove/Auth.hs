{-# LANGUAGE RecordWildCards #-}

module Auth (server, tcpAllowAXFR) where

import DNS.Auth.Algorithm
import DNS.Log
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

peerOf :: SockAddr -> String
peerOf sa = maybe (show sa) (show . fst) $ fromSockAddr sa

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
                more <- handleLogErr env WARNING True $ go query
                when more $ loop 0
    -- Returns whether there is any point in reading another message.
    go (bs, sa) =
        case decode bs of
            Left e -> do
                envPutLines
                    DEBUG
                    Nothing
                    ["undecodable message from " ++ peerOf sa ++ "/" ++ protoName ++ ": " ++ show e]
                -- No reply: 'decode' is all or nothing, so we do not
                -- have an identifier to answer FORMERR under.  Leaving
                -- a connection open after that only has the peer wait
                -- for the idle timeout, so it ends here.
                return $ not recvErrorFatal
            Right query -> (>> return True) $ case opcode query of
                OP_NOTIFY -> handleNotify proto zoneAlist sa query
                OP_STD -> do
                    let q = question query
                        dom = qname q
                        typ = qtype q
                        peer = peerOf sa
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
                            mx <- allowAXFR sa bs query zoneAlist
                            case mx of
                                TransferOk zone mmac ->
                                    transfer env proto zone mmac sa query
                                TransferRefused ->
                                    sendReply sa $ replyRefused proto query
                                TransferNotAuth e -> do
                                    envPutLines
                                        WARNING
                                        Nothing
                                        [ "    axfr @"
                                            ++ peer
                                            ++ "/TCP \""
                                            ++ toRepresentation dom
                                            ++ "\": "
                                            ++ show e
                                        ]
                                    sendReply sa $ replyNotAuth proto query
                        else
                            response proto zoneAlist sa query dom
                _ -> sendReply sa $ replyRefused proto query

response :: Proto -> ZoneAlist -> SockAddr -> DNSMessage -> Domain -> IO ()
response proto@Proto{..} zoneAlist sa query dom = case findZoneAlist dom zoneAlist of -- isSubDomainOf
    Nothing -> sendReply sa $ replyRefused proto query
    Just (_, zoneref) -> do
        zone <- readIORef zoneref
        -- A zone whose source could not be loaded holds the empty
        -- database, whose apex is the root, so every name in it would be
        -- answered with an authoritative NXDOMAIN and the placeholder
        -- SOA of that database.  We would be denying the existence of
        -- names we simply know nothing about, and downstream caches
        -- would keep the denial.
        if zoneReady zone
            then sendReply sa $ replyQuery proto query $ zoneDB zone
            else sendReply sa $ replyServFail proto query

handleNotify :: Proto -> ZoneAlist -> SockAddr -> DNSMessage -> IO ()
handleNotify proto@Proto{..} zoneAlist sa query = case lookup dom zoneAlist of -- exact match
    Nothing -> sendReply sa $ replyRefused proto query
    Just zoneref -> do
        Zone{..} <- readIORef zoneref
        case fromSockAddr sa of
            Nothing -> sendReply sa $ replyRefused proto query
            Just (ip, _)
                | ip `elem` zoneAllowNotifyAddrs -> do
                    sendReply sa $ replyNotice proto query
                    zoneWakeUp
                | otherwise -> sendReply sa $ replyRefused proto query
  where
    dom = qname $ question query

replyNotice :: Proto -> DNSMessage -> ByteString
replyNotice proto query = encodeReply proto query $ fromQuery query

replyQuery :: Proto -> DNSMessage -> DB -> ByteString
replyQuery proto query db = encodeReply proto query $ getAnswer db query

replyRefused :: Proto -> DNSMessage -> ByteString
replyRefused proto query = encodeReply proto query $ (fromQuery query){rcode = Refused}

-- | The TSIG on the request was not good (RFC 8945 Sec 5.2).  The
--   answer ought to carry a TSIG of its own saying which of the checks
--   failed; it does not yet, so a peer is told that it was not
--   authorised without being told why.  The log says why.
replyNotAuth :: Proto -> DNSMessage -> ByteString
replyNotAuth proto query = encodeReply proto query $ (fromQuery query){rcode = NotAuth}

-- | We are configured for this zone but have nothing to say about it.
--   Not authoritative: there is no data to be authoritative about.
replyServFail :: Proto -> DNSMessage -> ByteString
replyServFail proto query = encodeReply proto query reply{rcode = ServFail, flags = flgs}
  where
    reply = fromQuery query
    flgs = (flags reply){authAnswer = False}

----------------------------------------------------------------

-- | Encoding a reply for the transport it is going to be sent over.
encodeReply :: Proto -> DNSMessage -> DNSMessage -> ByteString
encodeReply Proto{..} query reply = case replyLimit query of
    Nothing -> encode reply
    Just lim -> fitIn lim reply

-- | Making a reply fit into the space the transport allows.
--
--   RFC 2181 Sec 9: the TC bit should not be set merely because some
--   additional data did not fit, so that section goes first and the
--   answer is still sent as a complete one.  Only when the answer
--   itself does not fit is TC set, with the sections emptied, so that
--   the client asks again over TCP.
fitIn :: Int -> DNSMessage -> ByteString
fitIn lim reply
    | BS.length whole <= lim = whole
    | BS.length noAdditional <= lim = noAdditional
    | otherwise = encode truncated
  where
    whole = encode reply
    noAdditional = encode reply{additional = []}
    truncated =
        reply
            { flags = (flags reply){trunCation = True}
            , answer = []
            , authority = []
            , additional = []
            }

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
import System.Posix.Time (epochTime)

import Axfr
import DNS.TSIG
import DNS.Types.Time (EpochTime)
import Exception
import TSIGKeys
import Types
import Zone

-- | How many receive errors in a row are tolerated before the server
--   starts to back off.
recvErrorBurst :: Int
recvErrorBurst = 10

-- | How long to pause once a socket keeps failing.
recvRetryDelay :: Int
recvRetryDelay = 1000000

currentTime :: IO EpochTime
currentTime = fromIntegral . fromEnum <$> epochTime

peerOf :: SockAddr -> String
peerOf sa = maybe (show sa) (show . fst) $ fromSockAddr sa

server :: Env -> TSIGKeys -> Proto -> ZoneAlist -> IO ()
server env@Env{..} keys proto@Proto{..} zoneAlist = loop 0
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
                OP_NOTIFY -> handleNotify env proto zoneAlist sa bs query
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
                                TransferNotAuth fault -> do
                                    envPutLines
                                        WARNING
                                        Nothing
                                        [ "    axfr @"
                                            ++ peer
                                            ++ "/TCP \""
                                            ++ toRepresentation dom
                                            ++ "\": "
                                            ++ show fault
                                        ]
                                    now <- currentTime
                                    sendReply sa $ replyNotAuth proto query fault now
                        else do
                            -- RFC 8945 Sec 5.2: a query which carries a
                            -- TSIG is checked before it is answered, and
                            -- Sec 5.3 has the answer to it carry one in
                            -- turn.  A query without one is answered as
                            -- it always was.
                            esealed <- sealFor env keys proto bs query
                            case esealed of
                                Left refusal -> sendReply sa refusal
                                Right seal -> response proto seal zoneAlist sa query dom
                _ -> sendReply sa $ replyRefused proto query

response :: Proto -> Seal -> ZoneAlist -> SockAddr -> DNSMessage -> Domain -> IO ()
response Proto{..} seal zoneAlist sa query dom = case findZoneAlist dom zoneAlist of -- isSubDomainOf
    Nothing -> sendReply sa $ seal $ refusal query
    Just (_, zoneref) -> do
        zone <- readIORef zoneref
        -- A zone whose source could not be loaded holds the empty
        -- database, whose apex is the root, so every name in it would be
        -- answered with an authoritative NXDOMAIN and the placeholder
        -- SOA of that database.  We would be denying the existence of
        -- names we simply know nothing about, and downstream caches
        -- would keep the denial.
        if zoneReady zone
            then sendReply sa $ seal $ getAnswer (zoneDB zone) query
            else sendReply sa $ seal $ serverFailure query

handleNotify :: Env -> Proto -> ZoneAlist -> SockAddr -> ByteString -> DNSMessage -> IO ()
handleNotify env proto@Proto{..} zoneAlist sa whole query = case lookup dom zoneAlist of -- exact match
    Nothing -> refuse
    Just zoneref -> do
        Zone{..} <- readIORef zoneref
        case zoneAllowNotifyKey of
            -- Holding the key is what says who this is, so the
            -- addresses are not asked about as well.
            Just key -> do
                now <- currentTime
                let held n = if n == tsigKeyName key then Just key else Nothing
                case verifyTSIG held now Nothing whole query of
                    TSIGOk mac -> do
                        sendReply sa $ replySigned proto query key now mac
                        zoneWakeUp
                    TSIGMissing -> refuse
                    TSIGFailed fault -> do
                        envPutLines
                            env
                            WARNING
                            Nothing
                            ["    notify " ++ peerOf sa ++ " \"" ++ toRepresentation dom ++ "\": " ++ show fault]
                        sendReply sa $ replyNotAuth proto query fault now
            Nothing -> case fromSockAddr sa of
                Just (ip, _)
                    | ip `elem` zoneAllowNotifyAddrs -> do
                        sendReply sa $ replyNotice proto query
                        zoneWakeUp
                _ -> refuse
  where
    dom = qname $ question query
    refuse = sendReply sa $ replyRefused proto query

replyNotice :: Proto -> DNSMessage -> ByteString
replyNotice proto query = encodeReply proto query $ fromQuery query

-- | The same, signed, so that whoever asked can tell we are who we say.
--
--   The MAC is taken over the plain encoding rather than over whatever
--   'encodeReply' would make of it.  The two are the same for anything
--   which fits, and an acknowledgement is nowhere near not fitting.
replySigned :: Proto -> DNSMessage -> TSIGKey -> EpochTime -> Opaque -> ByteString
replySigned proto query key now requestMAC = encodeReply proto query signedReply
  where
    reply = fromQuery query
    (rr, _) = signTSIG key now defaultFudge (Just requestMAC) (encode reply)
    signedReply = reply{additional = [rr]}

replyRefused :: Proto -> DNSMessage -> ByteString
replyRefused proto query = encodeReply proto query $ refusal query

-- | We will not answer this one.
refusal :: DNSMessage -> DNSMessage
refusal query = (fromQuery query){rcode = Refused}

-- | The TSIG on the request was not good (RFC 8945 Sec 5.2).  The
--   answer carries a TSIG of its own saying which of the checks failed,
--   so that the peer is told why it was not authorised rather than only
--   that it was not.
--
--   Where the answer is signed at all -- which is where the clocks
--   disagree and nowhere else -- the MAC is taken over the plain
--   encoding rather than over whatever 'encodeReply' would make of it,
--   as in 'replySigned'.  An answer with nothing in it but the question
--   is nowhere near not fitting.
replyNotAuth :: Proto -> DNSMessage -> TSIGFault -> EpochTime -> ByteString
replyNotAuth proto query fault now = encodeReply proto query reply{additional = [rr]}
  where
    reply = (fromQuery query){rcode = NotAuth}
    rr = errorTSIG fault now $ encode reply

-- | We are configured for this zone but have nothing to say about it.
--   Not authoritative: there is no data to be authoritative about.
serverFailure :: DNSMessage -> DNSMessage
serverFailure query = reply{rcode = ServFail, flags = flgs}
  where
    reply = fromQuery query
    flgs = (flags reply){authAnswer = False}

-- | The message is not one we can make sense of far enough to answer it
--   properly (RFC 8945 Sec 5.2: a TSIG anywhere but last is one of
--   those).
replyFormErr :: Proto -> DNSMessage -> ByteString
replyFormErr proto query = encodeReply proto query $ (fromQuery query){rcode = FormatErr}

----------------------------------------------------------------

-- | Closing off an answer: encoding it for the transport it goes over,
--   and signing it where the query it answers was signed.
type Seal = DNSMessage -> ByteString

-- | Looking at the TSIG on a query, where it has one (RFC 8945 Sec
--   5.2).  What comes back either closes off the answer -- signed with
--   the key the query came with, which Sec 5.3 requires of us -- or is
--   the whole of the answer, because the query carried a TSIG we would
--   not take.
--
--   The keys are the ones clove holds, all of them: a TSIG on an
--   ordinary query says who is asking, and answering is not a
--   permission that any of them grants.  What the key grants is said
--   elsewhere -- allow-transfer-key for a transfer, allow-notify-key
--   for a notify.
sealFor
    :: Env
    -> TSIGKeys
    -> Proto
    -> ByteString
    -> DNSMessage
    -> IO (Either ByteString Seal)
sealFor Env{..} keys proto whole query
    | not carried = return $ Right plain
    | otherwise = do
        now <- currentTime
        case verifyTSIG held now Nothing whole query of
            TSIGOk mac -> return $ case keyOf query of
                Just key -> Right $ sealWith proto query key now mac
                -- Unreachable: the check just found that key.
                Nothing -> Right plain
            -- Sec 5.2: exactly one record, and last.  Anything else is
            -- a message to answer FORMERR and no more.
            TSIGMissing -> return $ Left $ replyFormErr proto query
            TSIGFailed fault -> do
                envPutLines
                    WARNING
                    Nothing
                    ["    query \"" ++ toRepresentation (qname $ question query) ++ "\": " ++ show fault]
                return $ Left $ replyNotAuth proto query fault now
  where
    carried = any ((== TSIG) . rrtype) $ additional query
    held n = lookupTSIGKey n keys
    keyOf msg = lastTSIG msg >>= \(name, _) -> held name
    plain = encodeReply proto query

-- | The answer, signed with the key its query came with and bound to it
--   (RFC 8945 Sec 5.3).
--
--   Room for the record is taken out of what the transport allows
--   before the answer is made to fit, so that the signature is never
--   what pushes the answer over, and is never one of the things dropped
--   to bring it back under.  An answer which had to be truncated is
--   still signed: the far end is to be able to tell that the truncation
--   is ours.
sealWith :: Proto -> DNSMessage -> TSIGKey -> EpochTime -> Opaque -> Seal
sealWith Proto{..} query key now requestMAC reply = encode signedReply
  where
    fitted = fitting (subtract (tsigRoom key) <$> replyLimit query) reply
    (rr, _) = signTSIG key now defaultFudge (Just requestMAC) $ encode fitted
    signedReply = fitted{additional = additional fitted ++ [rr]}

----------------------------------------------------------------

-- | Encoding a reply for the transport it is going to be sent over.
encodeReply :: Proto -> DNSMessage -> DNSMessage -> ByteString
encodeReply Proto{..} query reply = encode $ fitting (replyLimit query) reply

-- | The reply as much of it as the transport leaves room for.
--
--   RFC 2181 Sec 9: the TC bit should not be set merely because some
--   additional data did not fit, so that section goes first and the
--   answer is still sent as a complete one.  Only when the answer
--   itself does not fit is TC set, with the sections emptied, so that
--   the client asks again over TCP.
fitting :: Maybe Int -> DNSMessage -> DNSMessage
fitting Nothing reply = reply
fitting (Just lim) reply
    | size reply <= lim = reply
    | size noAdditional <= lim = noAdditional
    | otherwise = truncated
  where
    size = BS.length . encode
    noAdditional = reply{additional = []}
    truncated =
        reply
            { flags = (flags reply){trunCation = True}
            , answer = []
            , authority = []
            , additional = []
            }

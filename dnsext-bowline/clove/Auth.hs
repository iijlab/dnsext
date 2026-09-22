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
            -- A response is not a question, and answering one is how
            -- two servers which both do it end up talking to each other
            -- until somebody stops them.  It is also how anybody who can
            -- forge a source address has us send a packet to whoever
            -- they name.  Nothing goes back.  Saying so at DEBUG rather
            -- than louder: a forged packet costs us nothing to drop, and
            -- a line of log for each one would cost more than the packet
            -- did.
            Right query
                | isResponse (flags query) -> do
                    envPutLines
                        DEBUG
                        Nothing
                        ["a response rather than a query from " ++ peerOf sa ++ "/" ++ protoName ++ ": ignored"]
                    return True
            Right query -> (>> return True) $ do
                -- RFC 8945 Sec 5.2: whatever the message is for, the
                -- TSIG on it is looked at first, and before anybody
                -- asks what holding that key is worth.
                echecked <- checkTSIG env keys proto sa bs query
                case echecked of
                    Left notAuth -> sendReply sa notAuth
                    Right sender -> do
                        -- Sec 5.3: and whatever we answer is signed with
                        -- the same key.
                        let seal = sealer proto query sender
                        case opcode query of
                            OP_NOTIFY -> handleNotify proto seal zoneAlist sa sender query
                            OP_STD -> do
                                let q = question query
                                    dom = qname q
                                    typ = qtype q
                                envPutLines
                                    DEBUG
                                    Nothing
                                    ["\"" ++ toRepresentation dom ++ "\" " ++ show typ ++ " from " ++ peerOf sa ++ "/" ++ protoName]
                                if typ == AXFR || typ == IXFR
                                    then do
                                        -- RFC 1995 Sec 4
                                        -- If incremental zone transfer is not
                                        -- available, the entire zone is returned.
                                        -- The first and the last RR of the response
                                        -- is the SOA record of the zone.  I.e. the
                                        -- behavior is the same as an AXFR response
                                        -- except the query type is IXFR.
                                        mx <- allowAXFR sa sender query zoneAlist
                                        case mx of
                                            TransferOk zone ->
                                                transfer env proto seal zone sender sa query
                                            TransferRefused ->
                                                sendReply sa $ seal $ refusal query
                                    else response proto seal zoneAlist sa query dom
                            _ -> sendReply sa $ seal $ refusal query

response :: Proto -> Seal -> ZoneAlist -> SockAddr -> DNSMessage -> Domain -> IO ()
response Proto{..} seal zoneAlist sa query dom = case findZoneFor (qtype $ question query) dom zoneAlist of -- isSubDomainOf
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
            then sendReply sa $ seal $ spoofed (zoneSpoof zone) $ getAnswer (zoneDB zone) query
            else sendReply sa $ seal $ serverFailure query

-- | Attaching to a reply what the zone was configured to send beyond
--   what it has to say.  Nothing at all unless clove was started with
--   @--insecure@, and nothing anybody would want in a server: a
--   resolver is being handed a delegation or an address which is
--   nobody's to give, so that a scenario can find out what it makes of
--   one.  The records go on the end, after the zone's own, and are not
--   signed -- neither is glue, which is why this cannot be seen through.
spoofed :: Spoof -> DNSMessage -> DNSMessage
spoofed Spoof{..} reply = attached $ denied reply
  where
    -- Only the rcode.  What the zone was going to say about the name is
    -- left where it is, the signed proof of it included, so a signed
    -- zone comes out saying one thing in the header and another below.
    denied r
        | qname (question r) `elem` spoofNxdomain = r{rcode = NXDomain}
        | otherwise = r
    attached r
        | null spoofAnswer && null spoofAuthority && null spoofAdditional = r
        | otherwise =
            r
                { answer = answer r ++ spoofAnswer
                , authority = authority r ++ spoofAuthority
                , additional = additional r ++ spoofAdditional
                }

-- | Someone says the zone has moved on (RFC 1996).  Whether to believe
--   them is what allow-notify-key and allow-notify-addrs decide; the
--   answer to a signed notify is signed either way.
handleNotify :: Proto -> Seal -> ZoneAlist -> SockAddr -> Sender -> DNSMessage -> IO ()
handleNotify Proto{..} seal zoneAlist sa sender query = case lookup dom zoneAlist of -- exact match
    Nothing -> refuse
    Just zoneref -> do
        Zone{..} <- readIORef zoneref
        case zoneAllowNotifyKey of
            -- Holding the key is what says who this is, so the
            -- addresses are not asked about as well.
            Just key
                | senderKey sender == Just key -> heard zoneWakeUp
                | otherwise -> refuse
            Nothing -> case fromSockAddr sa of
                Just (ip, _)
                    | ip `elem` zoneAllowNotifyAddrs -> heard zoneWakeUp
                _ -> refuse
  where
    dom = qname $ question query
    heard wake = do
        sendReply sa $ seal $ fromQuery query
        wake
    refuse = sendReply sa $ seal $ refusal query

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

-- | Looking at the TSIG on a message, where it has one (RFC 8945 Sec
--   5.2).  What comes back is either the key it was signed with, for
--   the answer to be signed with in turn, or the whole of the answer,
--   because it carried a TSIG we would not take.
--
--   The keys are the ones clove holds, all of them.  A TSIG says who is
--   talking, which is not the same question as what they may have, and
--   answering the first one here is what lets every kind of message be
--   answered properly signed -- including the ones whose permission
--   comes from an address rather than from a key.
checkTSIG
    :: Env
    -> TSIGKeys
    -> Proto
    -> SockAddr
    -> ByteString
    -> DNSMessage
    -> IO (Either ByteString Sender)
checkTSIG Env{..} keys proto@Proto{..} sa whole msg
    | not carried = return $ Right Unsigned
    | otherwise = do
        now <- currentTime
        case verifyTSIG held now Nothing whole msg of
            TSIGOk mac -> case lastTSIG msg of
                Just (name, rd)
                    | Just key <- held name -> do
                        -- Sec 5.2.3: and it must not be a message from
                        -- before the last one taken under this key.
                        fresh <- takeTSIGTime keys name (tsig_time_signed rd)
                        if fresh
                            then return $ Right $ SignedWith key mac now
                            else
                                refused now $
                                    TSIGFault
                                        { faultError = BADTIME
                                        , faultKeyName = name
                                        , faultRecord = rd
                                        , faultKey = Just key
                                        }
                -- Unreachable: the check just found that key.
                _ -> return $ Right Unsigned
            -- Sec 5.2: exactly one record, and last.  Anything else is
            -- a message to answer FORMERR and no more.
            TSIGMissing -> do
                envPutLines WARNING Nothing [said "a TSIG which is not one record at the end"]
                return $ Left $ replyFormErr proto msg
            TSIGFailed fault -> refused now fault
  where
    refused now fault = do
        envPutLines WARNING Nothing [said $ show fault]
        return $ Left $ replyNotAuth proto msg fault now
    said why =
        "    "
            ++ kind
            ++ " @"
            ++ peerOf sa
            ++ "/"
            ++ protoName
            ++ " \""
            ++ toRepresentation (qname $ question msg)
            ++ "\": "
            ++ why
    carried = any ((== TSIG) . rrtype) $ additional msg
    held n = lookupTSIGKey n keys
    kind = case opcode msg of
        OP_NOTIFY -> "notify"
        _ | qtype (question msg) `elem` [AXFR, IXFR] -> "axfr"
        _ -> "query"

-- | Closing off an answer for the message it answers.
sealer :: Proto -> DNSMessage -> Sender -> Seal
sealer proto query Unsigned = encodeReply proto query
sealer proto query (SignedWith key mac now) = sealWith proto query key now mac

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

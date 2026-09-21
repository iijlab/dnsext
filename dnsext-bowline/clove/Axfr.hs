{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Axfr (
    transfer,
    tcpAllowAXFR,
    client,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.Functor (($>))
import Data.IORef
import Data.IP
import qualified Data.IP.RouteTable as T
import Data.List as List
import Data.List.NonEmpty ()
import Data.Maybe
import Network.Socket
import qualified System.IO.Error as E
import System.Posix.Time (epochTime)
import System.Timeout (timeout)

import DNS.Auth.Algorithm
import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.TSIG
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import DNS.Types.Time (EpochTime)

import Exception
import Net
import Types

-- | Saying which zone and which upstream a failure belongs to.  Without
--   it the operator is told only that some socket somewhere would not
--   connect.
withUpstream :: IP -> PortNumber -> Domain -> String -> IO a -> IO a
withUpstream ip port dom what action = do
    er <- trySync action
    case er of
        Right a -> return a
        Left se ->
            E.ioError $
                E.userError $
                    what ++ " " ++ peer ip port dom ++ ": " ++ show se

peer :: IP -> PortNumber -> Domain -> String
peer ip port dom = "@" ++ show ip ++ "#" ++ show port ++ " \"" ++ toRepresentation dom ++ "\""

-- | Saying that we asked and did not get a usable answer.  'Nothing'
--   rather than an error: failing to reach the upstream this once is
--   something to carry on from, not to stop for.
unanswered :: Env -> IP -> PortNumber -> Domain -> String -> String -> IO (Maybe a)
unanswered Env{..} ip port dom what why = do
    envPutLines WARNING Nothing ["    " ++ what ++ " " ++ peer ip port dom ++ ": " ++ why]
    return Nothing

-- | Whether a transfer may go ahead.  The TSIG has been checked by the
--   time this is asked (RFC 8945 Sec 5.2); what is left is whether the
--   key it was signed with, or the address it came from, is one this
--   zone hands itself to.
tcpAllowAXFR :: SockAddr -> Sender -> DNSMessage -> ZoneAlist -> IO Transfer
tcpAllowAXFR sa sender msg zoneAlist = case List.lookup dom zoneAlist of -- exact match
    Nothing -> return TransferRefused
    Just zoneref -> do
        zone <- readIORef zoneref
        -- Transferring a zone which is not loaded would hand out the
        -- empty database, that is a zero record AXFR response.
        if not (zoneReady zone)
            then return TransferRefused
            else return $ case zoneTransferKey zone of
                -- Holding the key is what grants the transfer, so the
                -- addresses are not asked about as well.
                Just key
                    | senderKey sender == Just key -> TransferOk zone
                    | otherwise -> TransferRefused
                Nothing
                    | byAddress zone -> TransferOk zone
                    | otherwise -> TransferRefused
  where
    dom = qname $ question msg
    -- A route table is asked for one family or the other, so which
    -- one the peer is has to be decided first, and an IPv4 peer seen
    -- through a v6 socket is an IPv4 peer.  Without 'unmap' the
    -- address ::ffff:192.0.2.1 is looked for among the IPv6 ranges,
    -- where "allow-transfer-addrs: 192.0.2.1" is not, and the transfer
    -- is refused without a word about why.
    byAddress zone = case unmap . fst <$> fromSockAddr sa of
        Just (IPv4 ip4) -> fromMaybe False $ T.lookup (makeAddrRange ip4 32) t4
        Just (IPv6 ip6) -> fromMaybe False $ T.lookup (makeAddrRange ip6 128) t6
        Nothing -> False
      where
        t4 = zoneAllowTransfer4 zone
        t6 = zoneAllowTransfer6 zone

-- | Largest AXFR message clove builds, in octets.  Well inside the
--   65535 a TCP length prefix allows, and small enough to be going on
--   with.
--
--   It is not usually what decides how much goes in a message: the
--   encoder will not put more than a set number of distinct names in
--   one, and throws when asked to, which for a zone of many names comes
--   first.  Either way the answer is the same, to ask what actually
--   encodes rather than to work it out.
axfrLimit :: Int
axfrLimit = 16384

-- | Fewest bytes a resource record can take: a compressed owner name,
--   type, class, TTL and RDLENGTH.  Only used to bound the search
--   below, so that it never encodes far more records than could fit.
minRRSize :: Int
minRRSize = 12

-- | Handing the zone over, in as many messages as it takes.  Signed
--   with the key the request came with, where it came with one: RFC
--   8945 Sec 5.3 asks that of us whether the key is also what granted
--   the transfer or whether an address did.
transfer :: Env -> Proto -> Seal -> Zone -> Sender -> SockAddr -> DNSMessage -> IO ()
transfer Env{..} Proto{..} seal zone sender sa query = do
    room <- transferSlot
    case room of
        Nothing -> tooMany
        Just release -> handOver `E.finally` release
  where
    -- RFC 5936 has nothing to say for "not now", and this is not a
    -- refusal: the peer is allowed the zone and should come back for
    -- it, which is what a secondary does with a SERVFAIL at its retry
    -- interval.  Worth a line, since it is the configuration speaking.
    tooMany = do
        envPutLines
            NOTICE
            Nothing
            [ "    axfr @"
                ++ client'
                ++ "/TCP \""
                ++ zoneRep
                ++ "\": as many transfers as transfers-out allows are already going on"
            ]
        sendReply sa $ seal $ (fromQuery query){rcode = ServFail}

    handOver = do
        batches <- zoneCutUp zone reply
        sent <- newIORef (0 :: Int)
        let send bs = sendReply sa bs >> modifyIORef' sent (+ 1)
            -- What was written, once it has been written.  Said before
            -- the transfer began, it was a count of what we meant to
            -- send, and a transfer which did not finish -- a slow peer,
            -- a peer which stopped reading -- left a line saying it had.
            told what = do
                n <- readIORef sent
                envPutLines NOTICE Nothing ["    axfr @" ++ client' ++ "/TCP \"" ++ zoneRep ++ "\": " ++ what n]
            finished n = show n ++ " message(s)" ++ signedly
            unfinished n = "unfinished, " ++ show n ++ " of " ++ show (length batches) ++ " message(s)" ++ signedly
        (`E.onException` told unfinished) $ do
            duringTransfer $ case sender of
                Unsigned -> mapM_ (send . encode . withAnswer) batches
                SignedWith key requestMAC now -> signAndSend send now key (AtFirst $ Just requestMAC) batches
            told finished

    zoneRep = toRepresentation $ zoneName zone
    signedly = maybe "" (const ", signed") mkey
    reply = fromQuery query
    mkey = senderKey sender
    client' = maybe (show sa) (\(ip, port) -> show ip ++ "#" ++ show port) $ fromSockAddr sa
    withAnswer batch = reply{answer = batch}
    -- Measured in the shape it will be sent in.  A signed message
    -- carries a record with a name of its own, and the encoder counts
    -- the names in a message, so a batch measured without it can be one
    -- name too many with it.
    -- RFC 8945 Sec 5.3.1 allows most messages of a transfer to go
    -- unsigned; signing all of them is simpler and is what it asks for.
    signAndSend
        :: (BS.ByteString -> IO ())
        -> EpochTime
        -> TSIGKey
        -> Chain
        -> [[ResourceRecord]]
        -> IO ()
    signAndSend _ _ _ _ [] = return ()
    signAndSend send now key chain (batch : rest) = do
        let body = encode $ withAnswer batch
            (rr, mac) = case chain of
                -- The first message answers the request and is bound to
                -- it; every one after is bound to the one before.
                AtFirst mrequest -> signTSIG key now defaultFudge mrequest body
                AfterFirst prior _ -> signTSIGCont key now defaultFudge prior [body]
        send $ encode (withAnswer batch){additional = [rr]}
        signAndSend send now key (AfterFirst mac []) rest

-- | A TSIG of the largest a TSIG can be: a key name of the longest a
--   name may be, the longest algorithm name there is, and an untruncated
--   SHA-512 MAC.
--
--   The cut below is measured with this rather than with the key the
--   request came with, so that it does not depend on who is asking and
--   can be worked out once for the zone.  A real signature is never
--   larger, so a message which fits with this fits with that; what it
--   costs is the few hundred octets of slack in each message, out of
--   'axfrLimit'.
widestTSIG :: ResourceRecord
widestTSIG = tsigPlaceholder $ TSIGKey longestName HMAC_SHA512 BS.empty
  where
    -- 255 octets on the wire: four labels, three of the 63 a label may
    -- hold and one of 61, and the root.
    longestName = fromRepresentation $ intercalate "." [label 63, label 63, label 63, label 61] ++ "."
    label n = replicate n 'a'

-- | The zone cut into messages, worked out once and kept.
--
--   It used to be worked out for every request.  The cut is decided by
--   asking the encoder what fits, which for each message is a handful
--   of encodings of up to 'axfrLimit' octets, so the zone is encoded
--   something like ten times over: 0.31 s of processor for a zone of
--   sixty thousand records and 1.07 s for one of a hundred and eighty
--   thousand, every time anybody asked.  Fifty requests at once left an
--   ordinary query waiting eleven seconds behind them.
zoneCutUp :: Zone -> DNSMessage -> IO [[ResourceRecord]]
zoneCutUp zone reply = do
    cached <- readIORef $ zoneBatches zone
    case cached of
        Just batches -> return batches
        Nothing -> do
            batches <- axfrBatches asSent $ dbAll $ zoneDB zone
            -- Two transfers starting together may both do this, and
            -- both get the same answer; the second write is the same
            -- as the first.
            writeIORef (zoneBatches zone) $ Just batches
            return batches
  where
    -- Of the size and the names the real record will have, which is
    -- all the measuring needs of it.
    asSent batch = (reply{answer = batch}){additional = [widestTSIG]}

-- | Spreading the records of a zone over as many messages as they need.
--   RFC 5936 Sec 2.2 lets a transfer be split anywhere so long as it
--   opens and closes with the SOA, which dbAll already arranges; one
--   message only ever held as much as fit, which for a zone of a few
--   hundred records was none of it.
axfrBatches :: ([ResourceRecord] -> DNSMessage) -> [ResourceRecord] -> IO [[ResourceRecord]]
axfrBatches asSent = go
  where
    go [] = return []
    go rrs = do
        n <- fitting rrs
        let (batch, rest) = splitAt n rrs
        (batch :) <$> go rest
    -- As many records as stay within the limit, or a single record when
    -- even that does not: better an oversized message than no progress.
    fitting rrs = do
        one <- fits rrs 1
        if not one
            then return 1
            else search rrs 1 $ max 1 $ min (length rrs) (axfrLimit `div` minRRSize)
    search rrs lo hi
        | lo >= hi = return lo
        | otherwise = do
            let mid = (lo + hi + 1) `div` 2
            ok <- fits rrs mid
            if ok then search rrs mid hi else search rrs lo (mid - 1)
    -- The encoder throws rather than going over its own limits, so a
    -- batch it will not encode is a batch that does not fit.
    fits rrs n = do
        e <- trySync $ E.evaluate $ BS.length $ encode $ asSent $ take n rrs
        return $ either (const False) (<= axfrLimit) e

----------------------------------------------------------------

-- | Transferring the zone when the upstream has something newer than
--   the serial given.  Whether it answered at all is told apart from
--   what it said, since a zone which is not being refreshed is a zone on
--   its way to expiring (RFC 1035 Sec 3.3.13).  A transfer which starts
--   and then fails throws instead.
client
    :: Env
    -> Maybe TSIGKey
    -> Maybe Serial
    -> IP
    -> PortNumber
    -> Domain
    -> IO FromUpstream
client env mkey Nothing ip port dom = Transferred <$> axfrQuery env mkey ip port dom
client env mkey (Just serial0) ip port dom = do
    mserial <- serialQuery env mkey ip port dom
    case mserial of
        Nothing -> return Unreachable
        Just serial
            | serial > serial0 -> Transferred <$> axfrQuery env mkey ip port dom
            | otherwise -> return Unchanged

-- | What serial the upstream holds, signed with the key the zone names
--   where it names one (RFC 8945).  Built here rather than asked of the
--   resolver: the resolver makes its own messages, which leaves nowhere
--   to put a TSIG and nothing to check the answer against.
--
--   Without a key this is what it always was, an unsigned question and
--   whatever comes back that matches it.  With one, an answer which is
--   not signed by that key is no answer: it would otherwise be the one
--   thing between two cloves which anybody could make up, and making it
--   up is enough to hold a transfer off or to bring one on.
serialQuery :: Env -> Maybe TSIGKey -> IP -> PortNumber -> Domain -> IO (Maybe Serial)
serialQuery env mkey ip port dom = withUpstream ip port dom "SOA" $ do
    now <- currentTime
    ident <- singleGenId
    (out, mrequestMAC) <- asked now ident
    eanswer <- askUDP serialTries serialTimeout ip port out $ taken ident now mrequestMAC
    case eanswer of
        Left why -> nope why
        -- The answer has been shown to be the upstream's, so now what it
        -- says is read -- and read once: one which says anything but
        -- NOERROR has answered, and asking it again would only be told
        -- the same thing three times.
        Right msg
            | rcode msg /= NoErr -> nope $ show $ rcode msg
            | otherwise -> serialOf msg
  where
    q = Question dom SOA IN
    qctl = rdFlag FlagClear <> doFlag FlagClear
    nope = unanswered env ip port dom "SOA"

    asked now ident = do
        let bare = encodeQuery ident q qctl
        case mkey of
            Nothing -> return (bare, Nothing)
            Just key -> case decode bare of
                Left e -> E.ioError $ E.userError $ show e
                Right m -> do
                    let body = encode m
                        (rr, mac) = signTSIG key now defaultFudge Nothing body
                    return (encode m{additional = additional m ++ [rr]}, Just mac)

    -- An answer is one carrying our identifier and our question, from
    -- the upstream we asked -- which the connected socket sees to -- and
    -- signed with the key where there is one.  Anything else is not an
    -- answer, whatever it says: a refusal is a thing anybody can send
    -- us, and the wait goes on until one arrives which is the
    -- upstream's.
    taken ident now mrequestMAC bs = do
        msg <- either (Left . show) Right $ decode bs
        case checkRespM q ident msg of
            Just e -> Left $ show e
            Nothing -> checked now mrequestMAC bs msg $> msg

    checked now mrequestMAC bs msg = case mkey of
        Nothing -> Right ()
        Just key -> case verifyTSIG (held key) now mrequestMAC bs msg of
            TSIGOk _ -> Right ()
            TSIGMissing -> Left "the answer is not signed"
            -- RFC 8945 Sec 5.4: an answer which says NOTAUTH carries an
            -- unsigned record naming the error, which is why one that
            -- does not verify is still worth reading -- for the log.
            TSIGFailed fault -> Left $ case tsigReported msg of
                Just e -> "the far end says " ++ show e
                Nothing -> show fault

    held key n = if n == tsigKeyName key then Just key else Nothing

    serialOf msg = case answer msg of
        [] -> nope "no SOA in the answer"
        soa : _ -> case fromRData $ rdata soa of
            Nothing -> nope "broken SOA"
            Just s -> return $ Just $ soa_serial s

-- | How long to wait for the upstream to say what serial it holds, and
--   how many times to ask.  What the resolver was set to before this
--   was ours to do.
serialTimeout :: Int
serialTimeout = 3 * 1000000

serialTries :: Int
serialTries = 3

-- | How long a whole transfer may take.  Generous: a large zone is many
--   messages, and the thread waiting for them is the zone's own.
axfrTimeout :: Int
axfrTimeout = 300 * 1000000

currentTime :: IO EpochTime
currentTime = fromIntegral . fromEnum <$> epochTime

-- | Asking for a zone and reading it to the end.
--
--   A transfer arrives as a run of messages, not one (RFC 5936 Sec
--   2.2), and reading only the first left everything past the first
--   message behind.  The connection is driven here rather than through
--   a resolver, which answers one message per question by nature.
axfrQuery :: Env -> Maybe TSIGKey -> IP -> PortNumber -> Domain -> IO [ResourceRecord]
axfrQuery _env mkey ip port dom = withUpstream ip port dom "AXFR" $ do
    now <- currentTime
    mrrs <- timeout axfrTimeout $ E.bracket (openTCP ip port) close $ request now
    case mrrs of
        Nothing -> E.ioError $ E.userError "timed out"
        Just rrs -> return rrs
  where
    q = Question dom AXFR IN
    qctl = rdFlag FlagClear <> doFlag FlagClear
    request now sock = do
        let bare = encodeQuery 0 q qctl
        (asked, mrequestMAC) <- case mkey of
            Nothing -> return (bare, Nothing)
            Just key -> case decode bare of
                Left e -> E.ioError $ E.userError $ show e
                Right m -> do
                    let (rr, mac) = signTSIG key now defaultFudge Nothing bare
                    return (encode m{additional = additional m ++ [rr]}, Just mac)
        sendVC (sendTCP sock) asked
        collect now sock BS.empty (AtFirst mrequestMAC) []
    -- The records come back reversed, so the head is the last one seen:
    -- the transfer is over once that is the closing SOA.
    collect now sock rest chain racc = do
        (bs, rest') <- recvMessage vcLimitMax sock rest
        msg <- case decode bs of
            Left e -> E.ioError $ E.userError $ show e
            Right m -> return m
        case rcode msg of
            NoErr -> return ()
            -- A refusal over the TSIG says which check went wrong, and
            -- that is the whole of what we can be told about it.
            rc -> E.ioError $ E.userError $ case tsigReported msg of
                Just e -> show rc ++ ": the far end says " ++ show e
                Nothing -> show rc
        let racc' = reverse (answer msg) ++ racc
            done = case racc' of
                closing : _ : _ -> rrtype closing == SOA
                _ -> False
        chain' <- checkChain mkey now chain bs msg done
        if done
            then return $ opening $ reverse racc'
            else collect now sock rest' chain' racc'
    -- RFC 5936 Sec 2.2: the first record is the SOA and the last is the
    -- same one again.  The copy at the end is dropped here, which is
    -- what the rest of clove expects of a zone.
    opening (soa : rrs)
        | rrtype soa == SOA = soa : init rrs
    opening _ = []

----------------------------------------------------------------

-- | How far along the signatures of a transfer we are (RFC 8945 Sec
--   5.3.1).
data Chain
    = -- | Nothing has come back yet.  The MAC of the request, which the
      --   first message is bound to, if it was signed.
      AtFirst (Maybe Opaque)
    | -- | The MAC of the last message which carried a record, and the
      --   messages since then which did not.
      AfterFirst Opaque [BS.ByteString]

-- | Most messages of a transfer may come unsigned, so long as the
--   first and the last do not.  RFC 8945 Sec 5.3.1 puts up with ninety
--   nine of them in a row.
unsignedRun :: Int
unsignedRun = 99

-- | Following the signatures across a transfer, message by message.
checkChain
    :: Maybe TSIGKey
    -> EpochTime
    -> Chain
    -> BS.ByteString
    -- ^ this message, exactly as it arrived
    -> DNSMessage
    -> Bool
    -- ^ whether this is the last message of the transfer
    -> IO Chain
checkChain Nothing _ chain _ _ _ = return chain
checkChain (Just key) now chain bs msg final = case chain of
    -- The first message answers the request and is bound to it, and it
    -- has to be signed before anything after it is allowed not to be.
    AtFirst mrequest
        | not signedHere -> failed "the first message of the transfer is not signed"
        | otherwise -> took $ verifyTSIG held now mrequest bs msg
    AfterFirst prior earlier
        | signedHere -> took $ verifyTSIGCont held now prior earlier bs msg
        | final -> failed "the last message of the transfer is not signed"
        | length earlier >= unsignedRun ->
            failed $ show (unsignedRun + 1) ++ " messages in a row went unsigned"
        | otherwise -> return $ AfterFirst prior (earlier ++ [bs])
  where
    signedHere = any ((== TSIG) . rrtype) $ additional msg
    took r = case r of
        TSIGOk mac -> return $ AfterFirst mac []
        TSIGMissing -> failed "the record went missing between looking and checking"
        TSIGFailed e -> failed $ show e
    held n = if n == tsigKeyName key then Just key else Nothing
    failed why = E.ioError $ E.userError $ "TSIG: " ++ why

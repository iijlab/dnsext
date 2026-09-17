{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Axfr (
    transfer,
    tcpAllowAXFR,
    client,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Data.IP
import qualified Data.IP.RouteTable as T
import Data.List as List
import Data.List.NonEmpty ()
import Data.Maybe
import Network.Socket
import qualified Network.Socket.ByteString as NSB
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

-- | Largest AXFR message clove builds.  A name compression pointer is
--   fourteen bits wide, so a message staying under 16384 bytes can
--   never need one that does not fit -- the encoder throws when it does
--   -- and it is well inside the 65535 a TCP length prefix allows.
axfrLimit :: Int
axfrLimit = 16384

-- | Fewest bytes a resource record can take: a compressed owner name,
--   type, class, TTL and RDLENGTH.  Only used to bound the search
--   below, so that it never encodes far more records than could fit.
minRRSize :: Int
minRRSize = 12

transfer :: Env -> Proto -> Zone -> SockAddr -> DNSMessage -> IO ()
transfer Env{..} Proto{..} zone sa query = do
    let db = zoneDB zone
        client' = maybe (show sa) (\(ip, port) -> show ip ++ "#" ++ show port) $ fromSockAddr sa
    msgs <- axfrMessages (fromQuery query) $ dbAll db
    envPutLines
        NOTICE
        Nothing
        [ "    axfr @"
            ++ client'
            ++ "/TCP \""
            ++ toRepresentation (zoneName zone)
            ++ "\": "
            ++ show (length msgs)
            ++ " message(s)"
        ]
    mapM_ (sendReply sa) msgs

-- | Spreading the records of a zone over as many messages as they need.
--   RFC 5936 Sec 2.2 lets a transfer be split anywhere so long as it
--   opens and closes with the SOA, which dbAll already arranges; one
--   message only ever held as much as fit, which for a zone of a few
--   hundred records was none of it.
axfrMessages :: DNSMessage -> [ResourceRecord] -> IO [BS.ByteString]
axfrMessages reply = go
  where
    go [] = return []
    go rrs = do
        n <- fitting rrs
        let (batch, rest) = splitAt n rrs
        (encode reply{answer = batch} :) <$> go rest
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
    -- The encoder throws when a name lands beyond the reach of a
    -- compression pointer, so a batch it cannot encode is one that does
    -- not fit.
    fits rrs n = do
        e <- trySync $ E.evaluate $ BS.length $ encode reply{answer = take n rrs}
        return $ either (const False) (<= axfrLimit) e

----------------------------------------------------------------

-- | Transferring the zone when the upstream has something newer than
--   the serial given.  'Nothing' means there is nothing to transfer --
--   because the upstream has not moved on, or because it could not be
--   asked.  It does not mean the zone is empty, and it is not an error:
--   a failing transfer throws instead.
client
    :: Env
    -> Maybe TSIGKey
    -> Maybe Serial
    -> IP
    -> PortNumber
    -> Domain
    -> IO (Maybe [ResourceRecord])
client env mkey Nothing ip port dom = Just <$> axfrQuery env mkey ip port dom
client env mkey (Just serial0) ip port dom = do
    mserial <- serialQuery env ip port dom
    case mserial of
        Nothing -> return Nothing
        Just serial
            | serial > serial0 -> Just <$> axfrQuery env mkey ip port dom
            | otherwise -> return Nothing

serialQuery :: Env -> IP -> PortNumber -> Domain -> IO (Maybe Serial)
serialQuery env@Env{..} ip port dom = withUpstream ip port dom "SOA" $ do
    emsg <- fmap replyDNSMessage <$> resolve renv q qctl
    case emsg of
        Left e -> unanswered env ip port dom "SOA" $ show e
        Right msg -> case answer msg of
            [] -> unanswered env ip port dom "SOA" "no SOA in the answer"
            soa : _ -> case fromRData $ rdata soa of
                Nothing -> unanswered env ip port dom "SOA" "broken SOA"
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
            , rinfoPort = port
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
        (bs, rest') <- recvMessage sock rest
        msg <- case decode bs of
            Left e -> E.ioError $ E.userError $ show e
            Right m -> return m
        case rcode msg of
            NoErr -> return ()
            rc -> E.ioError $ E.userError $ show rc
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

-- | Reading one length-prefixed message, keeping whatever was read past
--   it for the next one.
recvMessage :: Socket -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvMessage sock rest0 = do
    (lenbs, rest1) <- recvExactly sock 2 rest0
    recvExactly sock (fromIntegral $ decodeVCLength lenbs) rest1

recvExactly :: Socket -> Int -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
recvExactly sock n rest0 = go [rest0] (BS.length rest0)
  where
    go acc len
        | len >= n = return $ BS.splitAt n $ BS.concat $ reverse acc
        | otherwise = do
            bs <- NSB.recv sock $ max 2048 (n - len)
            if BS.null bs
                then E.ioError $ E.userError "the connection closed in mid message"
                else go (bs : acc) (len + BS.length bs)

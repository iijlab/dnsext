{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Iterative.Query.Do53Stub where

-- GHC packages
import Control.Concurrent (myThreadId)
import Control.Exception (SomeException (..), bracket, fromException, throwIO)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import System.Timeout (timeout)

-- dnsext-types
import DNS.Types
import DNS.Types.Decode

-- dnsext-utils
import qualified DNS.Log as Log
import qualified DNS.WorkerStats as WStats

-- dnsext packages
import DNS.Do53.Client (FlagOp (FlagClear), QueryControls, ednsEnabled)
import DNS.Do53.Internal

-- other packages
import Network.Socket
import qualified Network.Socket.ByteString as NSB

-- this package
import DNS.Iterative.Imports

-- | Check response for a matching identifier and question.  If we ever do
-- pipelined TCP, we'll need to handle out of order responses.  See:
-- https://tools.ietf.org/html/rfc7766#section-7
checkResp :: Bool -> Question -> Identifier -> DNSMessage -> Bool
checkResp mixed q seqno = isNothing . checkRespM' mixed q seqno

-- | 'checkRespM', and where the case of the name was mixed on purpose,
--   that it has come back the way it went out.  'Eq' on a name folds
--   case by design -- a name is the same name however it is written --
--   so the echo has to be looked at separately or not at all.
checkRespM' :: Bool -> Question -> Identifier -> DNSMessage -> Maybe DNSError
checkRespM' mixed q seqno resp
    | Just e <- checkRespM q seqno resp = Just e
    | mixed, not (sameCase (qname q) (qname $ question resp)) = Just QuestionMismatch
    | otherwise = Nothing

-- | Whether this is our answer with the case of the name folded: the
--   identifier and the question are right and only the way the name is
--   written is not.  A server which does that is one of those which
--   does not keep the case a name arrived in, and mixing it is worth
--   nothing against such a server.
foldedTheCase :: Question -> Identifier -> DNSMessage -> Bool
foldedTheCase q seqno resp = isNothing (checkRespM q seqno resp) && not (sameCase (qname q) (qname $ question resp))

-- | The question with the name written in lower case, which is how it
--   will come back from a server which folds.
asFolded :: Question -> Question
asFolded q = q{qname = fromWireLabels $ wireLabels $ qname q}

caseNoEDNS :: Reply -> QueryControls -> Maybe QueryControls
caseNoEDNS rply qctl0
    | rc == FormatErr && eh == NoEDNS && qctl /= qctl0 = Just qctl
    | otherwise = Nothing
  where
    ans = replyDNSMessage rply
    rc = rcode ans
    eh = ednsHeader ans
    qctl = ednsEnabled FlagClear <> qctl0

fromIOException :: String -> E.IOException -> DNSError
fromIOException tag ioe = NetworkFailure (SomeException ioe) tag

{- FOURMOLU_DISABLE -}
tryDNS :: String -> IO a -> IO (Either DNSError a)
tryDNS ~tag action =
    E.try action >>= either left (return . Right)
  where
    left se
        | Just (e :: DNSError)  <- fromException se  = return $ Left   e
        | Just (e :: IOError)   <- fromException se  = return $ Left $ fromIOException tag e
        | otherwise                                  = throwIO se
{- FOURMOLU_ENABLE -}

timeoutDNS' :: String -> Int -> IO a -> IO a
timeoutDNS' tag micro action = maybe (throwIO $ DNSErrorInfo TimeoutExpired tag) pure =<< timeout micro action

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | A resolver using UDP and TCP.
--   fallback once for NoEDNS case
udpTcpResolver1 :: OneshotResolver
udpTcpResolver1 ri@ResolveInfo{rinfoActions = ResolveActions{..}} q qctl0 = timeout' $ do
    WStats.setThreadId ractionBlockingStat =<< myThreadId
    er1 <- udpResolver1 ri q qctl0
    case er1 of
        e1@(Left {})                  -> return e1
        __@(Right rply1)  -> case caseNoEDNS rply1 qctl0 of
            Nothing                   -> handleTC rply1 qctl0
            Just qctl1            -> do
                er2 <- udpResolver1 ri q qctl1
                case er2 of
                    e2@(Left {})      -> return e2
                    __@(Right rply2)  -> handleTC rply2 qctl1
  where
    ~qtag = queryTag q (nameTag ri "UDP-TCP") qctl0
    timeout' = timeoutDNS' ("iter.udpTcpResolver1: " ++ qtag) ractionTimeoutTime
    handleTC rply qctl
        | tc         = tcpResolver1 ri q qctl
        | otherwise  = return $ Right rply
      where
        tc = trunCation $ flags $ replyDNSMessage rply

{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | one-shot UDP resolver
--   - ignoring rinfoUDPRetry
--   - no fallback for NoEDNS case
udpResolver1 :: OneshotResolver
udpResolver1 ri@ResolveInfo{rinfoActions = ra@ResolveActions{..}, ..} q qctl0 = do
    logNoShort qtag
    tryDNS qtag (go qctl0)
  where
    logNoShort s = unless ractionShortLog (blockingIO "log" $ ractionLog Log.DEMO Nothing [s])
    tag = nameTag ri "UDP"
    ~qtag = queryTag q tag qctl0
    blockingIO n = raBlockingIO ra ("udp-rslv." ++ n ++ ": " ++ qtag)
    sblockingIO sock n action = withSockBucket sock $ \sbucket ->
        blockingIO (n ++ "." ++ show sock ++ "." ++ show sbucket) action

    -- Using only one socket and the same identifier.
    go qctl = bracket open close_ $ \sock -> do
        ractionSetSockOpt sock
        let send bs = sblockingIO sock "send" (NSB.send sock bs)
            recv = sblockingIO sock "recv" (NSB.recv sock 2048)
        ident <- ractionGenId
        mrply <- sendQueryRecvAnswer q ident qctl send recv
        case mrply of
            Just rply -> return rply
            {- The name came back folded, so this server does not keep
               the case it was given and the mixture is worth nothing
               against it.  Ask again as it likes to be asked, with a
               fresh identifier: the one just used has been seen by
               whoever sent that answer. -}
            Nothing -> do
                ractionLog Log.DEBUG Nothing ["udpResolver1: ", show rinfoIP, " does not keep the case of a name; asking again without mixing it"]
                ident' <- ractionGenId
                mplain <- sendQueryRecvAnswer (asFolded q) ident' qctl send recv
                maybe (E.throwIO SequenceNumberMismatch) return mplain

    sendQueryRecvAnswer q' ident qctl send recv = do
        let qry = encodeQuery ident q' qctl
        _ <- send qry
        let tx = BS.length qry
        recvAnswer q' ident recv tx

    recvAnswer q' ident recv tx = do
        ans <- recv
        now <- ractionGetTime
        case decodeAt now ans of
            Left e -> do
                ractionLog Log.DEBUG Nothing $
                    let showHex8 w
                            | w >= 16 = showHex w
                            | otherwise = ('0' :) . showHex w
                        dumpBS = ("\"" ++) . (++ "\"") . foldr (\w s -> "\\x" ++ showHex8 w s) "" . BS.unpack
                     in ["udpResolver1.recvAnswer: decodeAt Left: ", show rinfoIP ++ ", ", dumpBS ans]
                E.throwIO e
            Right msg
                | checkResp (isJust ractionMixCase) q' ident msg -> do
                    let rx = BS.length ans
                    return $
                        Just
                            Reply
                                { replyTag = tag
                                , replyDNSMessage = msg
                                , replyTxBytes = tx
                                , replyRxBytes = rx
                                }
                | isJust ractionMixCase && foldedTheCase q' ident msg -> return Nothing
                -- Just ignoring a wrong answer.
                | otherwise -> do
                    ractionLog
                        Log.DEBUG
                        Nothing
                        ["udpResolver1.recvAnswer: checkResp error: ", show rinfoIP, ", ", show msg]
                    recvAnswer q' ident recv tx

    open = do
        let host = show rinfoIP
            port = show rinfoPort
            hints = defaultHints{addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG]}
        addr <- NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)
        E.bracketOnError (openSocket addr) close_ $ \s -> do
            let sa = addrAddress addr
            sblockingIO s "connect" (connect s sa)
            return s

    close_ s = sblockingIO s "close" (close s)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | A resolver using TCP.
tcpResolver1 :: OneshotResolver
tcpResolver1 ri@ResolveInfo{rinfoActions = ra@ResolveActions{..}, ..} q qctl =
    -- Using a fresh connection
    bracket open close_ $ \sock -> do
        ractionSetSockOpt sock
        let send bs = sendVC (\xs -> sblockingIO sock "sendTCP" $ sendTCP sock xs) bs
            recv = recvVC rinfoVCLimit $ sblockingIO sock "recvTCP" $ recvTCP sock
        vcResolver1 tag send recv ri q qctl
  where
    tag = nameTag ri "TCP"
    blockingIO n = raBlockingIO ra ("tcp-rslv." ++ n ++ ": " ++ fromNameTag tag)
    sblockingIO sock n action = withSockBucket sock $ \sbucket ->
        blockingIO (n ++ "." ++ show sock ++ "." ++ show sbucket) action
    open = blockingIO "openTCP" (openTCP rinfoIP rinfoPort)
    close_ s = sblockingIO s "close" (close s)
{- FOURMOLU_ENABLE -}

-- | Generic resolver for virtual circuit.
vcResolver1 :: NameTag -> (BS -> IO ()) -> IO BS -> OneshotResolver
vcResolver1 tag send recv ResolveInfo{rinfoActions = ResolveActions{..}} q qctl0 = do
    logNoShort qtag
    tryDNS qtag (go qctl0)
  where
    logNoShort s = unless ractionShortLog (ractionLog Log.DEMO Nothing [s])
    ~qtag = queryTag q tag qctl0
    go qctl = do
        mrply <- sendQueryRecvAnswer q qctl
        case mrply of
            Just rply -> return rply
            {- As over UDP: a server which gives the name back folded
               does not keep the case, so the mixture buys nothing
               against it and the question goes again as it was written
               in the first place. -}
            Nothing -> do
                ractionLog Log.DEBUG Nothing ["vcResolver1: the far end does not keep the case of a name; asking again without mixing it"]
                mplain <- sendQueryRecvAnswer (asFolded q) qctl
                maybe (E.throwIO SequenceNumberMismatch) return mplain

    sendQueryRecvAnswer q' qctl = do
        -- Using a fresh identifier.
        ident <- ractionGenId
        let qry = encodeQuery ident q' qctl
        _ <- send qry
        let tx = BS.length qry
        res <- recvAnswer q' ident tx
        return res

    recvAnswer q' ident tx = do
        bs <- recv
        now <- ractionGetTime
        case decodeAt now bs of
            Left e -> E.throwIO e
            Right msg -> case checkRespM' (isJust ractionMixCase) q' ident msg of
                Nothing ->
                    return $
                        Just
                            Reply
                                { replyTag = tag
                                , replyDNSMessage = msg
                                , replyTxBytes = tx
                                , replyRxBytes = BS.length bs
                                }
                Just err
                    | isJust ractionMixCase && foldedTheCase q' ident msg -> return Nothing
                    | otherwise -> E.throwIO err

withSockBucket :: Socket -> (Int -> IO a) -> IO a
withSockBucket sock k = do
    sbucket <- withFdSocket sock (\fd -> pure (fromIntegral $ fd `rem` 32)) :: IO Int
    k sbucket

raBlockingIO :: ResolveActions -> String -> IO a -> IO a
raBlockingIO ResolveActions{..} = WStats.blockingIO ractionBlockingStat

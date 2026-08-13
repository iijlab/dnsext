{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Iterative.Query.Do53Stub where

-- GHC packages
import Control.Exception (SomeException (..))
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
checkResp :: Question -> Identifier -> DNSMessage -> Bool
checkResp q seqno = isNothing . checkRespM q seqno

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
        | Just (e :: DNSError) <- E.fromException se  = return $ Left   e
        | Just (e :: IOError)  <- E.fromException se  = return $ Left $ fromIOException tag e
        | otherwise                                   = E.throw se
{- FOURMOLU_ENABLE -}

timeoutDNS' :: String -> Int -> IO a -> IO a
timeoutDNS' tag micro action = maybe (E.throwIO $ DNSErrorInfo TimeoutExpired tag) pure =<< timeout micro action

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | A resolver using UDP and TCP.
--   fallback once for NoEDNS case
udpTcpResolver1 :: OneshotResolver
udpTcpResolver1 ri@ResolveInfo{rinfoActions = ResolveActions{..}} q qctl0 = timeout' $ do
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

    -- Using only one socket and the same identifier.
    go qctl = E.bracket open close_ $ \sock -> do
        ractionSetSockOpt sock
        let send bs = blockingIO "send" (NSB.send sock bs)
            recv = blockingIO "recv" (NSB.recv sock 2048)
        ident <- ractionGenId
        sendQueryRecvAnswer ident qctl send recv

    sendQueryRecvAnswer ident qctl send recv = do
        let qry = encodeQuery ident q qctl
        _ <- send qry
        let tx = BS.length qry
        recvAnswer ident recv tx

    recvAnswer ident recv tx = do
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
                | checkResp q ident msg -> do
                    let rx = BS.length ans
                    return $
                        Reply
                            { replyTag = tag
                            , replyDNSMessage = msg
                            , replyTxBytes = tx
                            , replyRxBytes = rx
                            }
                -- Just ignoring a wrong answer.
                | otherwise -> do
                    ractionLog
                        Log.DEBUG
                        Nothing
                        ["udpResolver1.recvAnswer: checkResp error: ", show rinfoIP, ", ", show msg]
                    recvAnswer ident recv tx

    open = do
        let host = show rinfoIP
            port = show rinfoPort
            hints = defaultHints{addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG]}
        addr <- NE.head <$> getAddrInfo (Just hints) (Just host) (Just port)
        E.bracketOnError (openSocket addr) close_ $ \s -> do
            let sa = addrAddress addr
            blockingIO "connect" (connect s sa)
            return s

    close_ s = blockingIO "close" (close s)

-- | A resolver using TCP.
tcpResolver1 :: OneshotResolver
tcpResolver1 ri@ResolveInfo{rinfoActions = ra@ResolveActions{..}, ..} q qctl =
    -- Using a fresh connection
    E.bracket open close_ $ \sock -> do
        ractionSetSockOpt sock
        let send bs = sendVC (\xs -> blockingIO "sendTCP" $ sendTCP sock xs) bs
            recv = recvVC rinfoVCLimit $ blockingIO "recvTCP" $ recvTCP sock
        vcResolver1 tag send recv ri q qctl
  where
    tag = nameTag ri "TCP"
    blockingIO n = raBlockingIO ra ("tcp-rslv." ++ n ++ ": " ++ fromNameTag tag)
    open = blockingIO "openTCP" (openTCP rinfoIP rinfoPort)
    close_ s = blockingIO "close" (close s)

-- | Generic resolver for virtual circuit.
vcResolver1 :: NameTag -> (BS -> IO ()) -> IO BS -> OneshotResolver
vcResolver1 tag send recv ResolveInfo{rinfoActions = ResolveActions{..}} q qctl0 = do
    logNoShort qtag
    tryDNS qtag (go qctl0)
  where
    logNoShort s = unless ractionShortLog (ractionLog Log.DEMO Nothing [s])
    ~qtag = queryTag q tag qctl0
    go qctl = sendQueryRecvAnswer qctl

    sendQueryRecvAnswer qctl = do
        -- Using a fresh identifier.
        ident <- ractionGenId
        let qry = encodeQuery ident q qctl
        _ <- send qry
        let tx = BS.length qry
        res <- recvAnswer ident tx
        return res

    recvAnswer ident tx = do
        bs <- recv
        now <- ractionGetTime
        case decodeAt now bs of
            Left e -> E.throwIO e
            Right msg -> case checkRespM q ident msg of
                Nothing ->
                    return $
                        Reply
                            { replyTag = tag
                            , replyDNSMessage = msg
                            , replyTxBytes = tx
                            , replyRxBytes = BS.length bs
                            }
                Just err -> E.throwIO err

raBlockingIO :: ResolveActions -> String -> IO a -> IO a
raBlockingIO ResolveActions{..} = WStats.blockingIO ractionBlockingStat

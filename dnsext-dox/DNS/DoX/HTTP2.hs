{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.HTTP2 (
    http2PersistentResolver,
    http2Resolver,
    http2cPersistentResolver,
    http2cResolver,
    doHTTP,
    doHTTPOneshot,
)
where

import qualified Control.Exception as E
import DNS.Do53.Client
import DNS.Do53.Internal
import qualified DNS.Log as Log
import DNS.Types
import DNS.Types.Decode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Short (fromShort)
import qualified Data.ByteString.Short as Short
import Network.HTTP.Types
import Network.HTTP2.Client (Client, Response, SendRequest, getResponseBodyChunk, requestBuilder, responseStatus)
import qualified Network.HTTP2.Client as H2
import qualified Network.HTTP2.TLS.Client as H2TLS

import DNS.DoX.Imports
import DNS.DoX.TLS

http2PersistentResolver :: PersistentResolver
http2PersistentResolver ri@ResolveInfo{..} body = toDNSError "http2PersistentResolver" $ do
    -- TLS SNI
    settings <- makeSettings ri tag
    ident <- ractionGenId rinfoActions
    withHandshakeTimeout ri $ \established ->
        H2TLS.runWithConfig config settings ipstr rinfoPort $
            onceUp established $
                doHTTP tag ident ri body
  where
    tag = nameTag ri "H2"
    ipstr = show rinfoIP
    -- HTTP :authority
    config = H2.defaultClientConfig{H2.authority = fromMaybe ipstr rinfoServerName}

http2Resolver :: OneshotResolver
http2Resolver ri@ResolveInfo{..} q qctl = toDNSError "http2Resolver" $ do
    settings <- makeSettings ri tag
    ident <- ractionGenId rinfoActions
    withHandshakeTimeout ri $ \established ->
        H2TLS.runWithConfig config settings ipstr rinfoPort $
            onceUp established $
                doHTTPOneshot tag ident ri q qctl
  where
    tag = nameTag ri "H2"
    ipstr = show rinfoIP
    -- HTTP :authority
    config = H2.defaultClientConfig{H2.authority = fromMaybe ipstr rinfoServerName}

http2cPersistentResolver :: PersistentResolver
http2cPersistentResolver ri@ResolveInfo{..} body = toDNSError "http2cPersistentResolver" $ do
    ident <- ractionGenId rinfoActions
    withHandshakeTimeout ri $ \established ->
        H2TLS.runH2CWithConfig config H2TLS.defaultSettings ipstr rinfoPort $
            onceUp established $
                doHTTP tag ident ri body
  where
    tag = nameTag ri "H2C"
    ipstr = show rinfoIP
    -- HTTP :authority
    config = H2.defaultClientConfig{H2.authority = fromMaybe ipstr rinfoServerName}

http2cResolver :: OneshotResolver
http2cResolver ri@ResolveInfo{..} q qctl = toDNSError "http2cResolver" $ do
    let tag = nameTag ri "H2C"
    ident <- ractionGenId rinfoActions
    withHandshakeTimeout ri $ \established ->
        H2TLS.runH2CWithConfig config H2TLS.defaultSettings ipstr rinfoPort $
            onceUp established $
                doHTTPOneshot tag ident ri q qctl
  where
    ipstr = show rinfoIP
    -- HTTP :authority
    config = H2.defaultClientConfig{H2.authority = fromMaybe ipstr rinfoServerName}

resolv
    :: NameTag
    -> Word16
    -> ResolveInfo
    -> SendRequest
    -> Resolver
resolv tag ident ResolveInfo{..} sendRequest q qctl = do
    sendRequest req $ \rsp -> do
        when (responseStatus rsp /= Just ok200) $ E.throwIO OperationRefused
        bs <- recvHTTP2 rsp
        now <- getTime
        case decodeAt now bs of
            Left e -> E.throwIO e
            Right msg -> case checkRespM q ident msg of -- fixme
                Nothing -> return $ Right $ Reply tag msg tx $ BS.length bs
                Just err -> return $ Left err
  where
    getTime = ractionGetTime rinfoActions
    wire = encodeQuery ident q qctl
    tx = BS.length wire
    hdr = clientDoHHeaders tx
    path = case rinfoPath of
        Nothing -> "/dns-query"
        Just p -> fromShort $ Short.takeWhile (/= 0x7b) p -- '{'
    req = requestBuilder methodPost path hdr $ BB.byteString wire

-- | Telling 'withHandshakeTimeout' that the connection is up, which is
--   what being called at all means for a 'Client'.
onceUp :: IO () -> Client a -> Client a
onceUp established client sendRequest aux = established >> client sendRequest aux

recvHTTP2 :: Response -> IO ByteString
recvHTTP2 rsp = go id
  where
    go build = do
        bs <- getResponseBodyChunk rsp
        if BS.null bs
            then
                return $ BS.concat $ build []
            else
                go (build . (bs :))

doHTTP
    :: NameTag
    -> Word16
    -> ResolveInfo
    -> (Resolver -> IO a)
    -> Client a
doHTTP tag ident ri body sendRequest _aux =
    body $ \q ctl -> withTimeout' to $ resolv tag ident ri sendRequest q ctl
  where
    to = ractionTimeoutTime $ rinfoActions ri

doHTTPOneshot
    :: NameTag
    -> Identifier
    -> ResolveInfo
    -> Question
    -> QueryControls
    -> Client (Either DNSError Reply)
doHTTPOneshot tag ident ri@ResolveInfo{..} q qctl sendRequest _aux = do
    ractionLog rinfoActions Log.DEMO Nothing [qtag]
    withTimeout ri $ resolv tag ident ri sendRequest q qctl
  where
    ~qtag = queryTag q tag qctl

clientDoHHeaders :: Int -> RequestHeaders
clientDoHHeaders len =
    [ (hUserAgent, "HaskellQuic/0.0.0")
    , (hContentType, "application/dns-message")
    , (hAccept, "application/dns-message")
    , (hContentLength, C8.pack $ show len)
    ]

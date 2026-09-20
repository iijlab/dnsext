{-# LANGUAGE OverloadedStrings #-}

module HTTP2Spec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import DNS.Do53.Internal
import DNS.DoX.Internal
import DNS.Types
import DNS.Types.Decode (decode)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString)
import Data.Either (isRight)
import Data.IORef
import Network.HTTP.Types
import Network.HTTP2.Server
import qualified Network.HTTP2.TLS.Server as H2TLS
import Network.Socket
import Test.Hspec

spec :: Spec
spec = describe "an answer over HTTP/2" $ do
    -- Every other transport bounds what it reads with rinfoVCLimit.
    -- DoH read the body to its end whatever its length, so a server
    -- which kept writing was answered by a client which kept reading:
    -- 200MB arrived in 0.3s and took the resolver to 767MB of resident
    -- memory.
    it "is not read past the limit" $
        withH2CServer (bodyOf $ 64 * 1024) $ \port -> do
            outcome <- outcomeOf $ http2cResolver (resolveInfo port) theQuestion mempty
            outcome `shouldContain` "over the limit"

    -- A Resolver promises not to throw, so its caller does not catch.
    -- resolv threw a refusal, a decoding failure and a timeout all the
    -- same, and the exception went out through the body of the
    -- persistent resolver, taking the connection and every other query
    -- on it down with the one which failed.
    it "reports a refusal rather than throwing it away with the connection" $ do
        turn <- newIORef (0 :: Int)
        answers <- newIORef []
        withH2CServer (refuseThenEcho turn) $ \port -> do
            _ <- outcomeOf $
                http2cPersistentResolver (resolveInfo port) $ \resolver -> do
                    -- No catching here: this is what the type promises.
                    resolver theQuestion mempty >>= keep answers
                    resolver theQuestion mempty >>= keep answers
            got <- reverse <$> readIORef answers
            case got of
                [Left refused, Right _] -> show refused `shouldContain` "OperationRefused"
                _ -> expectationFailure $ "two answers were expected, got " ++ show got

    -- RFC 8484 Sec 4.1: "In order to maximize HTTP cache friendliness,
    -- DoH clients using media type application/dns-message SHOULD use a
    -- DNS ID of 0 in every DNS request."  A random one went out
    -- instead, drawn once for the whole connection and then put on
    -- every question asked over it.
    it "asks with the identifier RFC 8484 asks for" $ do
        seen <- newIORef []
        withH2CServer (echoKeeping seen) $ \port -> do
            r <- http2cResolver (resolveInfo port) theQuestion mempty
            r `shouldSatisfy` isRight
            reverse <$> readIORef seen `shouldReturn` [0]

    it "asks with it on every question of a connection" $ do
        seen <- newIORef []
        withH2CServer (echoKeeping seen) $ \port -> do
            _ <- outcomeOf $
                http2cPersistentResolver (resolveInfo port) $ \resolver -> do
                    _ <- resolver theQuestion mempty
                    _ <- resolver theQuestion mempty
                    pure ()
            reverse <$> readIORef seen `shouldReturn` [0, 0]

keep :: IORef [a] -> a -> IO ()
keep ref x = modifyIORef' ref (x :)

theQuestion :: Question
theQuestion = Question "www.example.com" A IN

resolveInfo :: PortNumber -> ResolveInfo
resolveInfo port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 1000000}
        }

-- | Whatever came of it, said in one line: these resolvers report a
--   failure by throwing as often as by returning it.
outcomeOf :: Show a => IO a -> IO String
outcomeOf action = (show <$> action) `E.catch` handler
  where
    handler se@(E.SomeException _)
        | Just (E.SomeAsyncException _) <- E.fromException se = E.throwIO se
        | otherwise = return $ show se

-- | A server which answers with a body of the given length.
bodyOf :: Int -> Server
bodyOf len _req _aux sendResponse = sendResponse rsp []
  where
    rsp = responseBuilder ok200 hdr $ byteString $ BS.replicate len 0
    hdr = [(hContentType, "application/dns-message")]

-- | A server which answers by handing the question back, which is
--   enough to be taken for an answer, and keeps the identifier it was
--   asked with.
echoKeeping :: IORef [Identifier] -> Server
echoKeeping seen req _aux sendResponse = do
    wire <- wholeBody req
    case decode wire of
        Left _ -> pure ()
        Right msg -> keep seen $ identifier msg
    sendResponse (responseBuilder ok200 dnsHeaders $ byteString wire) []

dnsHeaders :: ResponseHeaders
dnsHeaders = [(hContentType, "application/dns-message")]

-- | A server which refuses the first question and echoes the second
--   back as its own answer, which is enough to be taken for one.
refuseThenEcho :: IORef Int -> Server
refuseThenEcho turn req _aux sendResponse = do
    n <- atomicModifyIORef' turn $ \n -> (n + 1, n)
    if n == 0
        then sendResponse (responseNoBody notFound404 []) []
        else do
            wire <- wholeBody req
            sendResponse (responseBuilder ok200 hdr $ byteString wire) []
  where
    hdr = [(hContentType, "application/dns-message")]

wholeBody :: Request -> IO BS.ByteString
wholeBody req = go id
  where
    go build = do
        bs <- getRequestBodyChunk req
        if BS.null bs
            then return $ BS.concat $ build []
            else go (build . (bs :))

withH2CServer :: Server -> (PortNumber -> IO a) -> IO a
withH2CServer server body = E.bracket open close $ \sock -> do
    port <- socketPort sock
    E.bracket (forkIO $ run sock) killThread $ \_ -> body port
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen sock 5
        return sock
    run sock = H2TLS.runH2CWithSocket H2TLS.defaultSettings sock server

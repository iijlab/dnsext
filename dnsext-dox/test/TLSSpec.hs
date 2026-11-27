{-# LANGUAGE OverloadedStrings #-}

module TLSSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (forever, void)
import DNS.Do53.Internal
import DNS.DoX.Internal
import DNS.Types
import Data.ByteString (ByteString)
import Network.Socket
import Network.TLS
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "a persistent resolver over TLS" $ do
    -- The sender and the receiver shared one give-up: the sender, done
    -- with a query, reset it to never and took the receiver's with it.
    -- The receiver then waited on a silent server for ever, and so did
    -- the resolver whose body had long returned.
    it "ends after its body even when the server never answers" $
        withSilentTLSServer $ \port -> do
            r <- timeout 5000000 $
                tlsPersistentResolver (resolveInfo port) $ \resolver ->
                    void $ resolver theQuestion mempty
            r `shouldBe` Just ()

theQuestion :: Question
theQuestion = Question "www.example.com" A IN

resolveInfo :: PortNumber -> ResolveInfo
resolveInfo port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoActions =
            defaultResolveActions
                { ractionTimeoutTime = 300000
                , ractionValidate = False
                }
        }

-- | A TLS server which completes the handshake, then reads and never
--   answers.
withSilentTLSServer :: (PortNumber -> IO a) -> IO a
withSilentTLSServer body = E.bracket open close $ \sock -> do
    port <- socketPort sock
    E.bracket (forkIO $ acceptLoop sock) killThread $ \_ -> body port
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen sock 5
        return sock
    acceptLoop sock = forever $ do
        (s, _) <- accept sock
        void $ forkIO $ serve s `E.finally` close s
    serve s = ignore $ do
        ctx <- contextNew s params
        handshake ctx
        forever $ recvData ctx
    ignore :: IO () -> IO ()
    ignore action = action `E.catch` \(E.SomeException _) -> return ()
    params =
        defaultParamsServer
            { serverShared = defaultShared{sharedCredentials = Credentials [cred]}
            }
    cred = either error id $ credentialLoadX509FromMemory certPEM keyPEM

certPEM :: ByteString
certPEM =
    "-----BEGIN CERTIFICATE-----\n\
    \MIIBfjCCASWgAwIBAgIUC8X21JT6Tdk3Z6J+BPNzKLHyN84wCgYIKoZIzj0EAwIw\n\
    \FDESMBAGA1UEAwwJbG9jYWxob3N0MCAXDTI2MTEyNzA4MjQyOFoYDzIxMjYxMTAz\n\
    \MDgyNDI4WjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwWTATBgcqhkjOPQIBBggqhkjO\n\
    \PQMBBwNCAAT2u7vzmWmVgILOEQcOfQX8Ab7feyCQDICzF76onm+Pj7Kv2Ch30J06\n\
    \YjM6r01f6nHDlaiyPmf2k9cE40vA2lXNo1MwUTAdBgNVHQ4EFgQUkpVnRPDk9Fuf\n\
    \CKszvrgj8o9BK/swHwYDVR0jBBgwFoAUkpVnRPDk9FufCKszvrgj8o9BK/swDwYD\n\
    \VR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAgNHADBEAiBRaXNQS0L+C4zCXmpsE7VQ\n\
    \geTmoU2zH5m7H3349bNvGAIgcVMmsuXHKnecdxMfvImRu3bLRYblKIjms42WClou\n\
    \i9M=\n\
    \-----END CERTIFICATE-----\n"

keyPEM :: ByteString
keyPEM =
    "-----BEGIN PRIVATE KEY-----\n\
    \MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgQu8g4lPnHI/IgSyc\n\
    \I/FxNGhN5TpWbwlV25/16ff16zmhRANCAAT2u7vzmWmVgILOEQcOfQX8Ab7feyCQ\n\
    \DICzF76onm+Pj7Kv2Ch30J06YjM6r01f6nHDlaiyPmf2k9cE40vA2lXN\n\
    \-----END PRIVATE KEY-----\n"

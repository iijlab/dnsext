{-# LANGUAGE OverloadedStrings #-}

module SetupSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import DNS.Do53.Internal
import DNS.DoX.Internal
import DNS.Types
import Data.IORef
import Data.Maybe (isJust)
import Network.Socket
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "connection setup" $ do
    -- Setting a connection up is not covered by the query timeout: a
    -- peer which accepts the connection and then says nothing leaves
    -- the resolver waiting for as long as it cares to hold the socket
    -- open.  TCP and h2c gave up after the query timeout and QUIC after
    -- its own handshake timeout, but DoT and DoH over TLS never came
    -- back at all.
    it "gives up on a DoT peer which never speaks TLS" $
        withSilentServer $ \port ->
            finishes $ tlsResolver (resolveInfo port) theQuestion mempty

    it "gives up on a DoT peer which never speaks TLS, persistently" $
        withSilentServer $ \port ->
            finishes $ tlsPersistentResolver (resolveInfo port) oneQuery

    it "gives up on an h2 peer which never speaks TLS" $
        withSilentServer $ \port ->
            finishes $ http2Resolver (resolveInfo port) theQuestion mempty

    it "gives up on an h2 peer which never speaks TLS, persistently" $
        withSilentServer $ \port ->
            finishes $ http2PersistentResolver (resolveInfo port) oneQuery

    it "gives up on an h2c peer which never answers" $
        withSilentServer $ \port ->
            finishes $ http2cResolver (resolveInfo port) theQuestion mempty

theQuestion :: Question
theQuestion = Question "www.example.com" A IN

oneQuery :: Resolver -> IO ()
oneQuery resolver = void $ resolver theQuestion mempty

resolveInfo :: PortNumber -> ResolveInfo
resolveInfo port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 1000000}
        }

-- | The action comes back one way or another, and soon.
finishes :: IO a -> Expectation
finishes action = do
    done <- timeout (5 * 1000 * 1000) $ swallow action
    done `shouldSatisfy` isJust

-- | Any answer will do, including a failure.  Asynchronous exceptions
--   are left alone so that 'timeout' can do its work.
swallow :: IO a -> IO ()
swallow action = void action `E.catch` handler
  where
    handler se@(E.SomeException _)
        | Just (E.SomeAsyncException _) <- E.fromException se = E.throwIO se
        | otherwise = return ()

-- | A server which accepts a connection and then says nothing.
withSilentServer :: (PortNumber -> IO a) -> IO a
withSilentServer body = E.bracket open shut $ \(sock, accepted) -> do
    port <- socketPort sock
    E.bracket (forkIO $ hold sock accepted) killThread $ \_ -> body port
  where
    open = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen sock 5
        ref <- newIORef []
        return (sock, ref)
    shut (sock, ref) = do
        close sock
        readIORef ref >>= mapM_ close
    -- The accepted sockets are held on to so that the garbage collector
    -- does not close them behind our back.
    hold sock ref = forever $ do
        (conn, _) <- accept sock
        modifyIORef' ref (conn :)

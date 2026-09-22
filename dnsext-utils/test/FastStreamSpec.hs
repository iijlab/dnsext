{-# LANGUAGE OverloadedStrings #-}

module FastStreamSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString ()
import Data.IORef
import Network.Run.TCP
import Network.Socket
import Test.Hspec

import DNS.TAP.FastStream

spec :: Spec
spec = do
    describe "reader & writer" $ do
        it "can send stream correctly in uni-directional" $ do
            let conf = Config False False
            readWrite conf
        it "can send stream correctly in bi-directional" $ do
            let conf = Config True False
            readWrite conf

readWrite :: Config -> IO ()
readWrite conf = do
    mvar <- newEmptyMVar
    -- The socket is listening before the client is started, so there is
    -- nothing to wait for: this used to sleep for ten milliseconds and
    -- hope, which is long enough on a quiet machine and not on a busy
    -- one -- "connect: does not exist (Connection refused)".
    E.bracket listening close $ \lsock -> do
        port <- socketPort lsock
        E.bracket (forkIO $ server mvar lsock) killThread $ \_ -> client mvar port
  where
    n = 10 :: Int
    client mvar port = runTCPClient "127.0.0.1" (show port) $ \sock -> do
        ref <- newIORef 0
        writer sock conf $ do
            i <- readIORef ref
            if i < n
                then do
                    let i' = i + 1
                    writeIORef ref i'
                    return "foo!"
                else return ""
        takeMVar mvar `shouldReturn` ()
    server mvar lsock = runTCPServerWithSocket lsock $ \sock -> do
        ref <- newIORef 0
        reader sock conf $ \_ -> modifyIORef' ref (+ 1)
        readIORef ref `shouldReturn` n
        putMVar mvar ()

-- | A socket listening on a port nothing else has.  The port used to be
--   written out, so the suite could not be run twice at once and shared
--   the number with whatever else on the machine had thought of it.
listening :: IO Socket
listening = do
    s <- socket AF_INET Stream defaultProtocol
    setSocketOption s ReuseAddr 1
    bind s $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
    listen s 10
    pure s

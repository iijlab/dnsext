{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UDPSpec (spec) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)
import Test.Hspec

import DNS.Iterative.Server
import Network.Socket

spec :: Spec
spec = describe "a UDP server" $ do
    -- A worker answering a server whose sender had been cut waited on its
    -- full queue for ever: the workers are shared by every server.
    it "drops answers once its sender is cut" $
        E.bracket open close $ \s -> do
            env <- newEmptyEnv
            inputs <- newEmptyMVar
            let toCacher inp = void $ tryPutMVar inputs inp
            [server] <- udpServers conf SynthNone env toCacher [s]
            done <- newEmptyMVar
            tid <- forkIO $ server `E.finally` putMVar done ()
            sa <- getSocketName s
            E.bracket (socket AF_INET Datagram defaultProtocol) close $ \c ->
                void $ NSB.sendTo c "query" sa
            Input{..} <- takeMVar inputs
            killThread tid
            takeMVar done
            let answer = inputToSender $ Output inputQuery inputPendingOp inputPeerInfo
            r <- timeout 2000000 $ replicateM_ 100 answer
            r `shouldBe` Just ()
  where
    open = do
        s <- socket AF_INET Datagram defaultProtocol
        bind s $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        return s
    conf =
        UdpServerConfig
            { udp_interface_automatic = False
            , udp_stop = retry
            , udp_drain = const $ return False
            }

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.TCP (
    tcpServers,
)
where

-- GHC packages
import Control.Concurrent.STM (atomically, writeTVar)
import qualified Control.Exception as E
import Control.Monad (join)
import Data.Functor
import Data.IORef

-- dnsext-utils packages
import qualified DNS.Log as Log
import qualified DNS.ThreadAsync as TAsync
import DNS.Transport.Types

-- dnsext-do53 packages
import qualified DNS.Do53.Internal as DNS

-- other packages
import Network.Run.TCP
import Network.Socket (getPeerName, getSocketName, waitAndCancelReadSocketSTM)
import qualified Network.Socket.ByteString as Network

-- this package
import DNS.Iterative.Internal (DNSQuery, FoldResponse, Env (..))
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Stop
import DNS.Iterative.Server.Types
import DNS.Iterative.Stats (incStatsTCP53, sessionStatsTCP53)

----------------------------------------------------------------

tcpServers :: VcServerConfig -> ServerActions
tcpServers conf synth env toCacher ss =
    concat <$> mapM (tcpServer conf env synth foldCached foldIterative toCacher) ss
  where
    (_synth, foldCached, foldIterative) = extendSynthesis synth

tcpServer
    :: VcServerConfig -> Env -> Synthesis
    -> (forall p . DNSQuery p -> FoldResponse IO p)
    -> (forall q . FoldResponse IO q)
    -> (ToCacher -> IO ()) -> Socket -> IO [IO ()]
tcpServer VcServerConfig{..} env synth foldCached foldIterative toCacher s = do
    name <- socketName s <&> (++ "/tcp")
    let tcpserver =
            withLocationIOE name $ do
                sessions <- newSessions
                stoppable vc_stop $ \install ->
                    runTCPServerWithSocketAndSettings
                        defaultServerSettings{settingsInstallShutdownHandler = install, settingsUnsafeClose = True}
                        s
                        (withSession sessions . go)
                waitSessions sessions
    return [tcpserver]
  where
    maxSize = fromIntegral vc_query_max_size
    tmicro = vc_idle_timeout * 1_000_000
    ignoreIOE :: E.IOException -> IO ()
    ignoreIOE _ = pure ()
    go sock = sessionStatsTCP53 (stats_ env) $ do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        logLn env Log.DEBUG $ "tcp-srv: accept: " ++ show peersa
        let peerInfo = PeerInfoVC peersa
        -- Each wait cancels the one before it, and the last is cancelled at the end.
        cancelRef <- newIORef $ pure ()
        let cancelWait = join (readIORef cancelRef) `E.catch` ignoreIOE
            waitRead = do
                cancelWait
                (readable, cancel) <- waitAndCancelReadSocketSTM sock
                writeIORef cancelRef cancel
                pure readable
        (vcSess, toSender, fromX) <- initVcSession waitRead
        -- Stopping the server ends the session as its idle timeout does,
        -- once what has already come is read and answered.
        let onStop = atomically $ stopped vc_stop >> writeTVar (vcStopping_ vcSess) True
        TAsync.withAsync "bw.tcp-stop" onStop $ \_ -> flip E.finally cancelWait $ withVcTimer tmicro (atomically $ enableVcTimeout $ vcTimeout_ vcSess) $ \vcTimer -> do
            let recv = Network.recv sock
            let onRecv bs = do
                    checkReceived vc_slowloris_size vcTimer bs
                    incStatsTCP53 peersa (stats_ env)
            let send = getSendVC vcTimer $ \bs _ -> DNS.sendVC (DNS.sendTCP sock) bs
                receiver = receiverVCnonBlocking "tcp-recv" env maxSize vcSess peerInfo recv onRecv toCacher
                           $ mkInput mysa toSender TCP synth foldCached foldIterative
                sender = senderVC "tcp-send" env vcSess send fromX
            TAsync.concurrently_ "bw.tcp-send" sender "bw.tcp-recv" receiver
        logLn env Log.DEBUG $ "tcp-srv: close: " ++ show peersa

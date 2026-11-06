{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.QUIC where

-- GHC packages
import Control.Concurrent.STM (atomically, isEmptyTQueue, writeTVar)

-- dnsext-utils
import qualified DNS.ThreadAsync as TAsync
import DNS.Transport.Types

-- dnsext-do53 packages
import qualified DNS.Do53.Internal as DNS

-- other packages
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Internal as QUIC
import Network.QUIC.Server (ServerConfig (..))
import qualified Network.QUIC.Server as QUIC
import Network.TLS (Credentials (..), SessionManager)

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Stop
import DNS.Iterative.Server.Types
import DNS.Iterative.Server.UDP
import DNS.Iterative.Stats (incStatsDoQ, sessionStatsDoQ)

----------------------------------------------------------------

quicServers :: VcServerConfig -> ServerActions
quicServers VcServerConfig{..} synth env toCacher ss = do
    -- fixme: withLocationIOE naming
    when vc_interface_automatic $ mapM_ setPktInfo ss
    let quicserver = withLocationIOE "QUIC" $ do
            sessions <- newSessions
            stoppableDraining vc_stop (vc_drain ss) sessions $ \draining install ->
                QUIC.runWithSockets ss sconf{scInstallShutdownHandler = install} $
                    withSession sessions . go draining
    return [quicserver]
  where
    (_synth, foldCached, foldIterative) = extendSynthesis synth
    tmicro = vc_idle_timeout * 1_000_000
    sconf = getServerConfig vc_credentials vc_session_manager "doq" (vc_idle_timeout * 1_000 + quicDeferMills) env
    quicDeferMills = 20 {- deferral until an exception is raised from quic library -}
    maxSize = fromIntegral vc_query_max_size
    go draining conn = sessionStatsDoQ (stats_ env) $ do
        info <- QUIC.getConnectionInfo conn
        let mysa = QUIC.localSockAddr info
            peersa = QUIC.remoteSockAddr info
            waitInput = return $ do
                isEmpty <- isEmptyTQueue $ QUIC.inputQ conn
                retryUntil $ not isEmpty
        (vcSess, toSender, fromX) <- initVcSession waitInput
        -- Draining ends the session as TCP's stopping does.
        let onDrain = atomically $ draining >> writeTVar (vcStopping_ vcSess) True
        TAsync.withAsync "bw.quic-drain" onDrain $ \_ -> withVcTimer tmicro (atomically $ enableVcTimeout $ vcTimeout_ vcSess) $ \vcTimer -> do
            let recv = do
                    strm <- QUIC.acceptStream conn
                    let peerInfo = PeerInfoStream peersa $ StreamQUIC strm
                    -- Without a designated thread, recvStream would block.
                    bs <- DNS.recvVC maxSize $ QUIC.recvStream strm 2048
                    checkReceived vc_slowloris_size vcTimer bs
                    incStatsDoQ peersa (stats_ env)
                    return (bs, peerInfo)
                send = getSendVC vcTimer $ \bs peerInfo -> do
                    case peerInfo of
                        PeerInfoStream _ (StreamQUIC strm) -> DNS.sendVC (QUIC.sendStreamMany strm) bs >> QUIC.closeStream strm
                        _ -> return ()
                -- FIXME
                receiver = receiverVC "quic-recv" env vcSess recv toCacher
                           $ mkInput mysa toSender DoQ synth foldCached foldIterative
                sender = senderVC "quic-send" env vcSess send fromX
            TAsync.concurrently_ "bw.quic-send" sender "bw.quic-recv" receiver

getServerConfig :: Credentials -> SessionManager -> ByteString -> Int -> Env -> ServerConfig
getServerConfig creds sm alpn tmills env =
    QUIC.defaultServerConfig
        { scALPN = Just (\_ bss -> if alpn `elem` bss then return alpn else return "")
        , scCredentials = creds
        , scUse0RTT = True
        , scSessionManager = sm
        , QUIC.scParameters = (QUIC.scParameters QUIC.defaultServerConfig){QUIC.maxIdleTimeout = fromIntegral tmills}
        , QUIC.scKeyLog = putSSLKeyLog_ env
        }

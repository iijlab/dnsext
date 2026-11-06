{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.TLS (
    tlsServers,
)
where

-- GHC packages
import Control.Applicative
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor

-- dnsext-utils
import qualified DNS.Log as Log
import qualified DNS.ThreadAsync as TAsync
import DNS.Transport.Types

-- dnsext-do53 packages
import qualified DNS.Do53.Internal as DNS

-- other packages
import qualified Network.HTTP2.TLS.Server as H2

-- this package
import DNS.Iterative.Internal (DNSQuery, FoldResponse, Env (..))
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Types
import DNS.Iterative.Stats (incStatsDoT, sessionStatsDoT)

tlsServers :: VcServerConfig -> ServerActions
tlsServers conf synth env toCacher ss =
    concat <$> mapM (tlsServer conf env synth foldCached foldIterative toCacher) ss
  where
    (_synth, foldCached, foldIterative) = extendSynthesis synth

type VsRunning = Bool

tlsServer :: VcServerConfig -> Env -> Synthesis
    -> (forall p . DNSQuery p -> FoldResponse IO p)
    -> (forall q . FoldResponse IO q)
    -> (ToCacher -> IO ()) -> Socket -> IO [IO ()]
tlsServer VcServerConfig{..} env synth foldCached foldIterative toCacher s = do
    name <- socketName s <&> (++ "/tls")
    let tlsserver = withLocationIOE name $ H2.runTLSWithSocket settings vc_credentials s "dot" go
    return [tlsserver]
  where
    maxSize = fromIntegral vc_query_max_size
    tmicro = vc_idle_timeout * 1_000_000
    settings =
        H2.defaultSettings
            { H2.settingsTimeout = vc_idle_timeout + 5 {- margin to avoid Timeout exception race -}
            , H2.settingsSlowlorisSize = vc_slowloris_size
            , H2.settingsSessionManager = vc_session_manager
            , H2.settingsEarlyDataSize = vc_early_data_size
            , H2.settingsKeyLogger = putSSLKeyLog_ env
            }
    go _ backend = sessionStatsDoT (stats_ env) $ do
        let mysa = H2.mySockAddr backend
            peersa = H2.peerSockAddr backend
            peerInfo = PeerInfoVC peersa
        logLn env Log.DEBUG $ "tls-srv: accept: " ++ show peersa
        vsRunning <- newTVarIO True
        inpq <- newTBQueueIO 8
        (vcSess, toSender, fromX, senderQ) <- initVcSession' (return $ checkInp inpq)

        {- bowline アプリ側: 将来的に STM 通信経由で切り離す -}
        TAsync.withAsync "bw.tls-recv-send" (
            withVcTimer tmicro (atomically $ enableVcTimeout $ vcTimeout_ vcSess) ( \vcTimer -> do
                let recv = getInp inpq
                let onRecv bs = do
                        checkReceived vc_slowloris_size vcTimer bs
                        incStatsDoT peersa (stats_ env)
                let send = getSendVC vcTimer $ \bs _ -> DNS.sendVC (H2.sendMany backend) bs
                    hrecv = exceptionCase $ \es -> logLn env Log.DEMO ("tls-recv: " ++ es)
                    receiver = hrecv $ receiverVCnonBlocking "tls-recv" env maxSize vcSess peerInfo recv onRecv toCacher
                               $ mkInput mysa toSender DoT synth foldCached foldIterative
                    hsend = exceptionCase $ \es -> do
                        let pendings = vcPendings_ vcSess
                        p <- showVcPendings pendings
                        logLn env Log.DEMO ("tls-send: " ++ es ++ ": " ++ p)
                        dequeueVcPendings pendings senderQ
                    sender = hsend $ senderVC "tls-send" env vcSess send fromX
                TAsync.concurrently_ "bw.tls-send" sender "bw.tls-recv" receiver
           )
           `finally`
           atomically (writeTVar vsRunning False)
          ) $ \_ ->
            reader backend (readTVar vsRunning) inpq
        logLn env Log.DEBUG $ "tls-srv: close: " ++ show peersa

reader :: H2.IOBackend -> STM VsRunning -> TBQueue ByteString -> IO ()
reader backend vsRunning inpq = loop
  where
    loop = do
        let proceed = do
                pkt <- H2.recv backend
                atomically $ writeTBQueue inpq pkt
                if pkt /= mempty
                    then loop
                    else atomically (guard . not =<< vsRunning)  -- wait session on EOF
        --
        next <- atomically $
                (guard . not =<< vsRunning)          $> return ()  -- breaks, not running
                <|>
                (guard . not =<< isFullTBQueue inpq) $> proceed    -- proceed only not full
        next

checkInp :: TBQueue ByteString -> STM ()
checkInp inpq = peekTBQueue inpq $> ()

getInp :: TBQueue ByteString -> Int -> IO ByteString
getInp inpq len = atomically $ do
    (bs1, bs2) <- BS.splitAt len <$> readTBQueue inpq
    when (bs2 /= mempty) $ unGetTBQueue inpq bs2
    return bs1

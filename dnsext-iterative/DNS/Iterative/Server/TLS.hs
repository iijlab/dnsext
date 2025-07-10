{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.TLS (
    tlsServers,
)
where

-- GHC packages

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor

-- dnsext-* packages
import qualified DNS.Do53.Internal as DNS
import qualified DNS.Log as Log
import DNS.TAP.Schema (SocketProtocol (..))
import qualified DNS.ThreadStats as TStat

-- other packages
import qualified Network.HTTP2.TLS.Server as H2

-- this package
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Server.NonBlocking
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Types
import DNS.Iterative.Stats (incStatsDoT, sessionStatsDoT)

tlsServers :: VcServerConfig -> ServerActions
tlsServers conf env toCacher ss =
    concat <$> mapM (tlsServer conf env toCacher) ss

tlsServer :: VcServerConfig -> Env -> (ToCacher -> IO ()) -> Socket -> IO [IO ()]
tlsServer VcServerConfig{..} env toCacher s = do
    name <- socketName s <&> (++ "/tls")
    let tlsserver = withLocationIOE name $ H2.runTLSWithSocket settings vc_credentials s "dot" go
    return [tlsserver]
  where
    tmicro = vc_idle_timeout * 1_000_000
    maxSize = fromIntegral vc_query_max_size
    settings =
        H2.defaultSettings
            { H2.settingsTimeout = vc_idle_timeout
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
        var <- newTVarIO ""
        inpq <- newTQueueIO
        (vcSess, toSender, fromX) <- initVcSession (return $ checkInp var inpq)
        E.bracket (forkIO $ reader backend inpq) killThread $ \_ -> do
            withVcTimer tmicro (atomically $ enableVcTimeout $ vcTimeout_ vcSess) $ \vcTimer -> do
                recv <- makeNBRecvVC maxSize $ getInp var inpq
                let onRecv bs = do
                        checkReceived vc_slowloris_size vcTimer bs
                        incStatsDoT peersa (stats_ env)
                let send = getSendVC vcTimer $ \bs _ -> DNS.sendVC (H2.sendMany backend) bs
                    receiver = receiverVCnonBlocking "tls-recv" env vcSess peerInfo recv onRecv toCacher $ mkInput mysa toSender DOT
                    sender = senderVC "tls-send" env vcSess send fromX
                TStat.concurrently_ "tls-send" sender "tls-recv" receiver
            logLn env Log.DEBUG $ "tls-srv: close: " ++ show peersa

reader :: H2.IOBackend -> TQueue ByteString -> IO ()
reader backend inpq = forever $ do
    pkt <- H2.recv backend
    atomically $ writeTQueue inpq pkt

checkInp :: TVar ByteString -> TQueue ByteString -> STM ()
checkInp var inpq = do
    bs0 <- readTVar var
    if bs0 /= ""
        then return ()
        else do
            isEmpty <- isEmptyTQueue inpq
            if isEmpty then retry else return ()

getInp :: TVar ByteString -> TQueue ByteString -> Int -> IO ByteString
getInp var inpq len = do
    bs0 <- atomically $ readTVar var
    bs <-
        if bs0 == ""
            then do
                atomically $ readTQueue inpq
            else
                return bs0
    let (bs1, bs2) = BS.splitAt len bs
    atomically $ writeTVar var bs2
    return bs1

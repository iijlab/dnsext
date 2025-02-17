{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.TCP (
    tcpServers,
)
where

-- GHC packages
import Control.Concurrent.STM (atomically)
import Data.Functor

-- dnsext-* packages
import qualified DNS.Do53.Internal as DNS
import qualified DNS.Log as Log
import DNS.TAP.Schema (SocketProtocol (..))
import qualified DNS.ThreadStats as TStat

-- other packages
import Network.Run.TCP
import Network.Socket (getPeerName, getSocketName, waitReadSocketSTM)

-- this package
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Server.NonBlocking
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Types
import DNS.Iterative.Stats (incStatsTCP53, sessionStatsTCP53)

----------------------------------------------------------------

tcpServers :: VcServerConfig -> ServerActions
tcpServers conf env toCacher ss =
    concat <$> mapM (tcpServer conf env toCacher) ss

tcpServer :: VcServerConfig -> Env -> (ToCacher -> IO ()) -> Socket -> IO ([IO ()])
tcpServer VcServerConfig{..} env toCacher s = do
    name <- socketName s <&> (++ "/tcp")
    let tcpserver =
            withLocationIOE name $
                runTCPServerWithSocket s go
    return ([tcpserver])
  where
    maxSize = fromIntegral vc_query_max_size
    tmicro = vc_idle_timeout * 1_000_000
    go sock = sessionStatsTCP53 (stats_ env) $ do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        logLn env Log.DEBUG $ "tcp-srv: accept: " ++ show peersa
        let peerInfo = PeerInfoVC peersa
        (vcSess, toSender, fromX) <- initVcSession (waitReadSocketSTM sock)
        withVcTimer tmicro (atomically $ enableVcTimeout $ vcTimeout_ vcSess) $ \vcTimer -> do
            recv <- makeNBRecvVCNoSize maxSize $ DNS.recvTCP sock
            let onRecv bs = do
                    checkReceived vc_slowloris_size vcTimer bs
                    incStatsTCP53 peersa (stats_ env)
            let send = getSendVC vcTimer $ \bs _ -> DNS.sendVC (DNS.sendTCP sock) bs
                receiver = receiverVCnonBlocking "tcp-recv" env vcSess peerInfo recv onRecv toCacher $ mkInput mysa toSender TCP
                sender = senderVC "tcp-send" env vcSess send fromX
            TStat.concurrently_ "tcp-send" sender "tcp-recv" receiver
        logLn env Log.DEBUG $ "tcp-srv: close: " ++ show peersa

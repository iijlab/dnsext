{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.HTTP2 (
    http2Servers,
    http2cServers,
    VcServerConfig (..),
    doHTTP,
) where

-- GHC packages
import qualified Control.Exception as E
import Control.Monad (forever)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Data.Functor

-- dnsext-utils packages
import qualified DNS.Log as Log
import qualified DNS.ThreadAsync as TAsync
import DNS.Transport.Types

-- other packages
import Data.ByteString.Base64.URL
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP2.Server as H2
import Network.HTTP2.TLS.Server (ServerIO (..), Stream)
import qualified Network.HTTP2.TLS.Server as H2TLS
import qualified Network.QUIC as QUIC

-- this package
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Types
import DNS.Iterative.Stats (incStatsDoH2, incStatsDoH2C, sessionStatsDoH2, sessionStatsDoH2C)

http2Servers :: VcServerConfig -> ServerActions
http2Servers conf env toCacher ss =
    concat <$> mapM (http2Server conf env toCacher) ss

http2Server :: VcServerConfig -> Env -> (ToCacher -> IO ()) -> Socket -> IO [IO ()]
http2Server VcServerConfig{..} env toCacher s = do
    name <- socketName s <&> (++ "/h2")
    let http2server = withLocationIOE name $ H2TLS.runIO settings vc_credentials s $ doHTTP "h2" sbracket incQuery env toCacher H2
    return [http2server]
  where
    sbracket = sessionStatsDoH2 (stats_ env)
    incQuery inet6 = incStatsDoH2 inet6 (stats_ env)
    settings =
        H2TLS.defaultSettings
            { H2TLS.settingsTimeout = vc_idle_timeout
            , H2TLS.settingsSlowlorisSize = vc_slowloris_size
            , H2TLS.settingsSessionManager = vc_session_manager
            , H2TLS.settingsEarlyDataSize = vc_early_data_size
            , H2TLS.settingsKeyLogger = putSSLKeyLog_ env
            }

http2cServers :: VcServerConfig -> ServerActions
http2cServers conf env toCacher ss =
    concat <$> mapM (http2cServer conf env toCacher) ss

http2cServer :: VcServerConfig -> Env -> (ToCacher -> IO ()) -> Socket -> IO [IO ()]
http2cServer VcServerConfig{..} env toCacher s = do
    name <- socketName s <&> (++ "/h2c")
    let http2server = withLocationIOE name $ H2TLS.runIOH2C settings s $ doHTTP "h2c" sbracket incQuery env toCacher H2C
    return [http2server]
  where
    sbracket = sessionStatsDoH2C (stats_ env)
    incQuery inet6 = incStatsDoH2C inet6 (stats_ env)
    settings =
        H2TLS.defaultSettings
            { H2TLS.settingsTimeout = vc_idle_timeout
            , H2TLS.settingsSlowlorisSize = vc_slowloris_size
            , H2TLS.settingsSessionManager = vc_session_manager -- not used
            }

class IsStream a where
    fromSuperStream :: SuperStream -> a
    toSuperStream :: a -> SuperStream

instance IsStream Stream where
    fromSuperStream (StreamH2 a) = a
    fromSuperStream _ = error ""
    toSuperStream = StreamH2

instance IsStream QUIC.Stream where
    fromSuperStream (StreamQUIC a) = a
    fromSuperStream _ = error ""
    toSuperStream = StreamQUIC

doHTTP
    :: IsStream a
    => String
    -> (IO () -> IO ())
    -> (SockAddr -> IO ())
    -> Env
    -> (ToCacher -> IO ())
    -> DoX
    -> ServerIO a
    -> IO (IO ())
doHTTP name sbracket incQuery env toCacher dox ServerIO{..} = do
    (toSender, fromX, _, _, senderQ, pendings) <- mkConnector'
    let hrecv = exceptionCase $ \es -> logLn env Log.DEMO (name ++ "-recv: " ++ es)
        rloop i = do
            (sprstrm, req) <- sioReadRequest
            ts <- currentTimeUsec_ env
            let peerInfo = PeerInfoStream sioPeerSockAddr (toSuperStream sprstrm)
            einp <- getInput req
            case einp of
                Left emsg -> logLn env Log.WARN $ "http.decode-error: " ++ name ++ ": " ++ emsg
                Right bs -> pendingOp pendings i $ \add pop -> do
                    let inp = mkInput sioMySockAddr toSender dox bs peerInfo pop ts
                    add
                    incQuery sioPeerSockAddr
                    toCacher inp
            rloop (i + 1)
        receiver = hrecv $ rloop 1
        --
        hsend = exceptionCase $ \es -> do
            p <- showVcPendings pendings
            logLn env Log.DEMO (name ++ "-send: " ++ es ++ ": " ++ p)
            dequeueVcPendings pendings senderQ
        sfinalize (Output _ VcPendingOp{..} _) = vpDelete
        sloop =
            forever $
                E.bracket fromX sfinalize $ \(Output bs' _ peerInfo) -> do
                    let ~(PeerInfoStream _ sprstrm) = peerInfo
                        header = mkHeader bs'
                        response = H2.responseBuilder HT.ok200 header $ byteString bs'
                    sioWriteResponse (fromSuperStream sprstrm) response
        sender = hsend sloop
        run = sbracket $ TAsync.concurrently_ ("bw." ++ name ++ "-send") sender ("bw." ++ name ++ "-recv") receiver
    return (run >> sioDone)
  where
    mkHeader bs =
        [ (HT.hContentType, "application/dns-message")
        , (HT.hContentLength, C8.pack $ show $ C8.length bs)
        ]

getInput :: H2.Request -> IO (Either String C8.ByteString)
getInput req
    | method == Just "GET" = case H2.requestPath req of
        Just path | "/dns-query?dns=" `C8.isPrefixOf` path -> return $ Right $ decodeLenient $ C8.drop 15 path
        _ -> return $ Left "illegal URL"
    | method == Just "POST" = do
        bs <- recvHTTP2 req
        return $ Right bs
    | otherwise = return $ Left "illegal method"
  where
    method = H2.requestMethod req

recvHTTP2 :: H2.Request -> IO C8.ByteString
recvHTTP2 req = go id
  where
    go build = do
        bs <- H2.getRequestBodyChunk req
        if C8.null bs
            then return $ C8.concat $ build []
            else go (build . (bs :))

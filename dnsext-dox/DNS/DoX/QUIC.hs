{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.QUIC where

import Codec.Serialise
import DNS.Do53.Internal
import DNS.Types.Decode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.QUIC
import Network.QUIC.Client
import Network.QUIC.Internal hiding (Recv, shared)
import qualified Network.TLS as TLS

import DNS.DoX.Imports
import DNS.DoX.SAN
import DNS.DoX.TLS
import qualified DNS.Log as Log

quicPersistentResolver :: PersistentResolver
quicPersistentResolver ri body = do
    cc <- getQUICParams ri tag "doq"
    toDNSError "quicPersistentResolver" $ run cc $ \conn -> do
        -- The query timeout, which quicResolver has had all along and
        -- which every other transport puts on each question.  Without
        -- it a peer which opens the connection and then leaves a stream
        -- unanswered is waited on for ever.
        body $ \q qctl -> withTimeout ri $ resolv conn ri q qctl
        saveResumptionInfo conn ri tag
  where
    tag = nameTag ri "QUIC"

quicResolver :: OneshotResolver
quicResolver ri q qctl = do
    cc <- getQUICParams ri tag "doq"
    toDNSError "quicResolver" $ run cc $ \conn -> withTimeout ri $ do
        res <- resolv conn ri q qctl
        saveResumptionInfo conn ri tag
        return res
  where
    tag = nameTag ri "QUIC"

resolv :: Connection -> ResolveInfo -> Resolver
resolv conn ri@ResolveInfo{..} q qctl = do
    strm <- stream conn
    ident <- ractionGenId rinfoActions
    let qry = encodeQuery ident q qctl
        tx = BS.length qry
    sendVC (sendStreamMany strm) qry
    shutdownStream strm
    bs <- recvVC rinfoVCLimit $ recvStream strm 2048
    now <- getTime
    case decodeAt now bs of
        Left e -> return $ Left e
        Right msg -> case checkRespM q ident msg of -- fixme
            Nothing -> return $ Right $ Reply tag msg tx $ BS.length bs
            Just err -> return $ Left err
  where
    getTime = ractionGetTime rinfoActions
    tag = nameTag ri "QUIC"

saveResumptionInfo :: Connection -> ResolveInfo -> NameTag -> IO ()
saveResumptionInfo conn ResolveInfo{..} tag = do
    rinfo <- getResumptionInfo conn
    when (isResumptionPossible rinfo) $ do
        let bs = BL.toStrict $ serialise rinfo
        ractionOnResumptionInfo rinfoActions tag bs

getQUICParams :: ResolveInfo -> NameTag -> ByteString -> IO ClientConfig
getQUICParams ResolveInfo{..} tag alpn0 = do
    resInfo <- ractionResumptionInfo rinfoActions tag
    let rinfo = case resInfo of
            [] -> defaultResumptionInfo
            r : _ -> case deserialiseOrFail $ BL.fromStrict r of
                Left _ -> defaultResumptionInfo
                Right x -> x
    return $
        defaultClientConfig
            { ccServerName = show rinfoIP
            , -- TLS SNI
              ccServerNameOverride = rinfoServerName
            , ccUseServerNameIndication = False
            , ccPortName = show rinfoPort
            , ccALPN = \_ -> return $ Just [alpn0]
            , ccDebugLog = False
            , ccValidate = ractionValidate rinfoActions
            , ccOnServerCertificate =
                makeOnServerCertificate (ractionLog rinfoActions Log.DEMO Nothing . (: [])) $ ractionServerAltName rinfoActions
            , ccResumption = rinfo
            , ccUse0RTT = ractionUseEarlyData rinfoActions
            , ccKeyLog = ractionKeyLog rinfoActions
            , ccHooks =
                defaultHooks
                    { onConnectionEstablished = \i -> do
                        let ~ver = if version i == Version1 then "v1" else "v2"
                            ~mode = case handshakeMode i of
                                TLS.PreSharedKey -> "Resumption"
                                TLS.RTT0 -> "0-RTT"
                                x -> show x
                            ~msg = ver ++ "(" ++ mode ++ ")"
                        ractionOnConnectionInfo rinfoActions tag msg
                    }
            , ccPacketSize = Just 1200
            }

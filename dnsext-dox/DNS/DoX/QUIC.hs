{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.QUIC where

import Control.Concurrent.STM (STM)
import qualified Control.Exception as E
import DNS.Do53.Internal
import DNS.Types (DNSError (TimeoutExpired))
import DNS.Types.Decode
import qualified Data.ByteString as BS
import Network.QUIC
import Network.QUIC.Client
import Network.QUIC.Internal hiding (Recv, shared)
import qualified Network.TLS as TLS

import DNS.DoX.GiveUp
import DNS.DoX.Imports
import DNS.DoX.SAN
import DNS.DoX.TLS
import qualified DNS.Log as Log

quicPersistentResolver :: PersistentResolver
quicPersistentResolver ri body = do
    cc <- getQUICParams ri tag "doq"
    toDNSError "quicPersistentResolver" $ runQUIC ri cc $ \conn -> do
        -- The query timeout, which quicResolver has had all along and
        -- which every other transport puts on each question.  Without
        -- it a peer which opens the connection and then leaves a stream
        -- unanswered is waited on for ever.
        body $ \q qctl -> queryGivingUp ri $ \g -> resolv conn ri g q qctl
        saveResumptionInfo conn ri tag
  where
    tag = nameTag ri "QUIC"

quicResolver :: OneshotResolver
quicResolver ri q qctl = do
    cc <- getQUICParams ri tag "doq"
    toDNSError "quicResolver" $ runQUIC ri cc $ \conn -> queryGivingUp ri $ \g -> do
        res <- resolv conn ri g q qctl
        saveResumptionInfo conn ri tag
        return res
  where
    tag = nameTag ri "QUIC"

-- | RFC 9250 Sec 4.2.1: "When sending queries over a QUIC connection,
--   the DNS Message ID MUST be set to 0."  The stream is what pairs a
--   query with its answer, so there is nothing left for an identifier
--   to do, and a peer which keeps to the RFC answers with zero whatever
--   we sent.
doqIdentifier :: Word16
doqIdentifier = 0

-- | Asking on a stream of its own, giving up on each step with
--   'TimeoutExpired' once the 'STM' action succeeds.
resolv :: Connection -> ResolveInfo -> STM () -> Resolver
resolv conn ri@ResolveInfo{..} giveUp q qctl = do
    strm <- orTimeout =<< streamWith conn giveUp
    let qry = encodeQuery doqIdentifier q qctl
        tx = BS.length qry
    sendVC (\bss -> orTimeout =<< sendStreamManyWith strm bss giveUp) qry
    orTimeout =<< shutdownStreamWith strm giveUp
    bs <- recvVC rinfoVCLimit $ orTimeout =<< recvStreamWith strm 2048 giveUp
    now <- getTime
    case decodeAt now bs of
        Left e -> return $ Left e
        Right msg -> case checkRespM q doqIdentifier msg of
            Nothing -> return $ Right $ Reply tag msg tx $ BS.length bs
            Just err -> return $ Left err
  where
    getTime = ractionGetTime rinfoActions
    tag = nameTag ri "QUIC"
    orTimeout :: Either () a -> IO a
    orTimeout = either (\_ -> E.throwIO TimeoutExpired) return

-- | Running a QUIC client whose handshake gives up at the query timeout,
--   with 'TimeoutExpired'.
runQUIC :: ResolveInfo -> ClientConfig -> (Connection -> IO a) -> IO a
runQUIC ri cc client = do
    gu <- connectGiveUp ri
    orTimeoutExpired =<< runGivingUp cc (\conn -> disarm gu >> client conn) (giveUpSTM gu)

saveResumptionInfo :: Connection -> ResolveInfo -> NameTag -> IO ()
saveResumptionInfo conn ResolveInfo{..} tag = do
    rinfo <- getResumptionInfo conn
    when (isResumptionPossible rinfo) $ do
        let bs = encodeResumptionInfo rinfo
        ractionOnResumptionInfo rinfoActions tag bs

getQUICParams :: ResolveInfo -> NameTag -> ByteString -> IO ClientConfig
getQUICParams ResolveInfo{..} tag alpn0 = do
    resInfo <- ractionResumptionInfo rinfoActions tag
    let rinfo = case resInfo of
            [] -> defaultResumptionInfo
            r : _ -> fromMaybe defaultResumptionInfo $ decodeResumptionInfo r
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.TLS where

import Codec.Serialise
import Control.Concurrent.STM (STM)
import qualified Control.Exception as E
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as BL
import Data.Either (rights)
import qualified Network.HTTP2.TLS.Client as H2TLS
import qualified Network.HTTP2.TLS.Internal as H2TLS
import Network.Run.TCP (openClientSocketGivingUp)
import Network.TLS

import DNS.Do53.Internal
import DNS.DoX.GiveUp
import DNS.DoX.Imports
import DNS.DoX.SAN
import qualified DNS.Log as Log
import DNS.Types

tlsPersistentResolver :: PersistentResolver
tlsPersistentResolver ri@ResolveInfo{..} body = toDNSError "tlsPersistentResolver" $ do
    (settings, gu) <- connectGivingUp ri =<< makeSettings ri tag
    -- Using a fresh connection
    asTimeout gu $ H2TLS.runTLS settings (show rinfoIP) rinfoPort "dot" $ \ctx _ _ -> do
        disarm gu
        let sendDoT g = givingUpOn gu g . sendVC (H2TLS.sendManyTLS ctx)
            -- connection timeout, not query timeout
            to = ractionTimeoutTime rinfoActions * 10
        -- One reader for the connection: recvTLS hands over a whole TLS
        -- record, so two answers written together arrive together and
        -- a reader made for each message would drop the second.
        recvVC' <- makeRecvVC rinfoVCLimit $ H2TLS.recvTLS ctx
        let recvDoT g = do
                d <- deadline to
                givingUpOn gu (g <|> d) recvVC' `E.catch` \GaveUp -> do
                    late <- gaveUp d
                    E.throwIO $ if late then E.toException TimeoutExpired else E.toException GaveUp
        vcPersistentResolver tag sendDoT recvDoT ri body
  where
    tag = nameTag ri "TLS"

makeSettings :: ResolveInfo -> NameTag -> IO H2TLS.Settings
makeSettings ResolveInfo{..} tag = do
    resInfos <- ractionResumptionInfo rinfoActions tag
    return $
        H2TLS.defaultSettings
            { H2TLS.settingsValidateCert = ractionValidate rinfoActions
            , H2TLS.settingsOnServerCertificate =
                makeOnServerCertificate (ractionLog rinfoActions Log.DEMO Nothing . (: [])) $ ractionServerAltName rinfoActions
            , H2TLS.settingsUseEarlyData = ractionUseEarlyData rinfoActions
            , -- TLS SNI
              H2TLS.settingsServerNameOverride = rinfoServerName
            , H2TLS.settingsUseServerNameIndication = False
            , H2TLS.settingsKeyLogger = ractionKeyLog rinfoActions
            , H2TLS.settingsWantSessionResumeList =
                rights (deserialiseOrFail . BL.fromStrict <$> resInfos)
            , H2TLS.settingsSessionManager =
                noSessionManager
                    { sessionEstablish = \sid sd -> do
                        let bs = BL.toStrict $ serialise (sid, sd)
                        ractionOnResumptionInfo rinfoActions tag bs
                        return Nothing
                    }
            , H2TLS.settingsOnServerFinished = \i -> do
                let ~ver = if infoVersion i == TLS13 then "v1.3" else "v1.2"
                    ~mode = case infoTLS13HandshakeMode i of
                        Nothing -> if infoTLS12Resumption i then "Resumption" else "FullHandshake"
                        Just PreSharedKey -> "Resumption"
                        Just RTT0 -> "0-RTT"
                        Just x -> show x
                    ~msg = ver ++ "(" ++ mode ++ ")"
                ractionOnConnectionInfo rinfoActions tag msg
            , -- intentionally 10 times larger
              H2TLS.settingsTimeout = ractionTimeoutTime rinfoActions `div` 100000
            }

tlsResolver :: OneshotResolver
tlsResolver ri@ResolveInfo{..} q qctl = toDNSError "tlsResolver" $ do
    (settings, gu) <- connectGivingUp ri =<< makeSettings ri tag
    -- Using a fresh connection
    asTimeout gu $ H2TLS.runTLS settings (show rinfoIP) rinfoPort "dot" $ \ctx _ _ -> do
        disarm gu
        let sendDoT g = givingUpOn gu g . sendVC (H2TLS.sendManyTLS ctx)
            recvDoT g = givingUpOn gu g $ recvVC rinfoVCLimit $ H2TLS.recvTLS ctx
        vcResolver tag sendDoT recvDoT ri q qctl
  where
    tag = nameTag ri "TLS"

-- | Giving up connecting and the TLS handshake at the query timeout.
--   'disarm' stops it once the connection is up.
connectGivingUp :: ResolveInfo -> H2TLS.Settings -> IO (H2TLS.Settings, GiveUp)
connectGivingUp ResolveInfo{..} settings = do
    gu <- newGiveUp =<< deadline (ractionTimeoutTime rinfoActions)
    let giveUp = giveUpSTM gu
        settings' =
            settings
                { H2TLS.settingsGiveUp = giveUp
                , H2TLS.settingsOpenClientSocket = openClientSocketGivingUp giveUp []
                }
    return (settings', gu)

-- | Running an operation of the connection with the 'GiveUp' pointed at
--   the 'STM' action, throwing 'GaveUp' once it has given up.
givingUpOn :: GiveUp -> STM () -> IO a -> IO a
givingUpOn gu g action =
    withGiveUp gu g action `E.catch` \e -> do
        given <- gaveUp g
        if given then E.throwIO GaveUp else E.throwIO (e :: E.SomeException)

-- | Giving up a question at the query timeout, answering 'TimeoutExpired'.
queryGivingUp
    :: ResolveInfo -> (STM () -> IO (Either DNSError a)) -> IO (Either DNSError a)
queryGivingUp ResolveInfo{..} action = do
    g <- deadline $ ractionTimeoutTime rinfoActions
    action g `E.catch` \e -> do
        given <- gaveUp g
        if given then return (Left TimeoutExpired) else E.throwIO (e :: E.SomeException)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.TLS where

import Codec.Serialise
import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as BL
import Data.Either (rights)
import qualified Network.HTTP2.TLS.Client as H2TLS
import qualified Network.HTTP2.TLS.Internal as H2TLS
import Network.TLS
import System.Timeout (timeout)

import DNS.Do53.Internal
import DNS.DoX.Imports
import DNS.DoX.SAN
import qualified DNS.Log as Log
import DNS.Types

tlsPersistentResolver :: PersistentResolver
tlsPersistentResolver ri@ResolveInfo{..} body = toDNSError "tlsPersistentResolver" $ do
    settings <- makeSettings ri tag
    -- Using a fresh connection
    withHandshakeTimeout ri $ \established ->
        H2TLS.runTLS settings (show rinfoIP) rinfoPort "dot" $ \ctx _ _ -> do
            established
            let sendDoT = sendVC $ H2TLS.sendManyTLS ctx
                -- connection timeout, not query timeout
                to = ractionTimeoutTime rinfoActions * 10
            -- One reader for the connection: recvTLS hands over a whole TLS
            -- record, so two answers written together arrive together and
            -- a reader made for each message would drop the second.
            recvVC' <- makeRecvVC rinfoVCLimit $ H2TLS.recvTLS ctx
            let recvDoT = withTimeout' to recvVC'
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
    settings <- makeSettings ri tag
    -- Using a fresh connection
    withHandshakeTimeout ri $ \established ->
        H2TLS.runTLS settings (show rinfoIP) rinfoPort "dot" $ \ctx _ _ -> do
            established
            let sendDoT = sendVC $ H2TLS.sendManyTLS ctx
                to = ractionTimeoutTime rinfoActions
                recvDoT = withTimeout' to $ recvVC rinfoVCLimit $ H2TLS.recvTLS ctx
            vcResolver tag sendDoT recvDoT ri q qctl
  where
    tag = nameTag ri "TLS"

withTimeout :: ResolveInfo -> IO (Either DNSError Reply) -> IO (Either DNSError Reply)
withTimeout ResolveInfo{..} action = do
    mres <- timeout (ractionTimeoutTime rinfoActions) action
    case mres of
        Nothing -> return $ Left TimeoutExpired
        Just res -> return res

withTimeout' :: Int -> IO a -> IO a
withTimeout' to action = do
    mres <- timeout to action
    case mres of
        Nothing -> E.throwIO TimeoutExpired
        Just res -> return res

-- | Running an action which has to set a connection up before it can do
--   anything.
--
--   Setting the connection up is outside the query timeout, which only
--   covers reading an answer once there is somewhere to read it from.
--   A peer which accepts the connection and then says nothing therefore
--   used to leave us in the TLS handshake for as long as it cared to
--   hold the socket open.  The timer here is stopped by the action
--   itself, with the @IO ()@ it is handed, as soon as the connection is
--   up, so that a connection which is meant to last is not cut off.
withHandshakeTimeout :: ResolveInfo -> (IO () -> IO a) -> IO a
withHandshakeTimeout ResolveInfo{..} action = do
    up <- newEmptyMVar
    caller <- myThreadId
    E.bracket (forkIO $ watch caller up) killThread $ \_ ->
        action $ void $ tryPutMVar up ()
  where
    watch caller up = do
        got <- timeout (ractionTimeoutTime rinfoActions) $ takeMVar up
        when (isNothing got) $ E.throwTo caller TimeoutExpired

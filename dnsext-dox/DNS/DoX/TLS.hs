{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.TLS where

import Control.Concurrent.STM (STM, retry)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Network.HTTP2.TLS.Client as H2TLS
import qualified Network.HTTP2.TLS.Internal as H2TLS
import Network.TLS

import DNS.Do53.Internal
import DNS.DoX.GiveUp
import DNS.DoX.Imports
import DNS.DoX.SAN
import qualified DNS.Log as Log
import DNS.Types

tlsPersistentResolver :: PersistentResolver
tlsPersistentResolver ri@ResolveInfo{..} body = toDNSError "tlsPersistentResolver" $ do
    settings <- makeSettings ri tag
    -- Using a fresh connection
    runDoT ri settings $ \ctx -> do
        let sendDoT g = sendVC $ sendDoTWith ctx g
            -- connection timeout, not query timeout
            to = ractionTimeoutTime rinfoActions * 10
        -- One reader for the connection: recvTLS hands over a whole TLS
        -- record, so two answers written together arrive together and
        -- a reader made for each message would drop the second.
        -- Only the receiver reads 'stopRef', which holds its give-up.
        stopRef <- newIORef retry
        recvVC' <- makeRecvVC rinfoVCLimit $ readIORef stopRef >>= recvDoTWith ctx
        let recvDoT g = do
                d <- deadline to
                (writeIORef stopRef (g <|> d) >> recvVC') `E.catch` \GaveUp -> do
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
                mapMaybe decodeTLSResumption resInfos
            , H2TLS.settingsSessionManager =
                noSessionManager
                    { sessionEstablish = \sid sd -> do
                        let bs = encodeTLSResumption sid sd
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
    runDoT ri settings $ \ctx -> do
        let sendDoT g = sendVC $ sendDoTWith ctx g
            recvDoT g = recvVC rinfoVCLimit $ recvDoTWith ctx g
        vcResolver tag sendDoT recvDoT ri q qctl
  where
    tag = nameTag ri "TLS"

-- | Giving up connecting and the TLS handshake at the query timeout.
--   'disarm' stops it once the connection is up.
connectGiveUp :: ResolveInfo -> IO GiveUp
connectGiveUp ResolveInfo{..} = newGiveUp =<< deadline (ractionTimeoutTime rinfoActions)

-- | DoT over a fresh connection, giving up connecting and the TLS
--   handshake with 'TimeoutExpired'.
runDoT :: ResolveInfo -> H2TLS.Settings -> (Context -> IO a) -> IO a
runDoT ri@ResolveInfo{..} settings action = do
    gu <- connectGiveUp ri
    let action' ctx _ _ = disarm gu >> action ctx
    orTimeoutExpired
        =<< H2TLS.runTLSGivingUp cliconf settings ipstr rinfoPort "dot" action' (giveUpSTM gu)
  where
    ipstr = show rinfoIP
    cliconf = H2TLS.defaultClientConfig settings $ H2TLS.defaultAuthority ipstr

-- | Sending, giving up on the 'STM' action with 'GaveUp'.  Each call
--   has its own, so the sender and the receiver do not share one.
sendDoTWith :: Context -> STM () -> [ByteString] -> IO ()
sendDoTWith ctx g bss =
    orGaveUp $ H2TLS.afterCloseNotifyWith $ sendDataWith ctx (LBS.fromChunks bss) g

-- | Receiving, giving up on the 'STM' action with 'GaveUp'.
recvDoTWith :: Context -> STM () -> IO ByteString
recvDoTWith ctx g = orGaveUp $ H2TLS.recvTLSWith ctx g

-- | Giving up a question at the query timeout, answering 'TimeoutExpired'.
queryGivingUp
    :: ResolveInfo -> (STM () -> IO (Either DNSError a)) -> IO (Either DNSError a)
queryGivingUp ResolveInfo{..} action = do
    g <- deadline $ ractionTimeoutTime rinfoActions
    action g `E.catch` \e -> do
        given <- gaveUp g
        if given && isSync e then return (Left TimeoutExpired) else E.throwIO e

-- | Others' asynchronous exceptions are not for giving up to take.
isSync :: E.SomeException -> Bool
isSync e = case E.fromException e of
    Just (E.SomeAsyncException _) -> False
    Nothing -> True

----------------------------------------------------------------

-- | What a DoT connection leaves for resuming: a version byte, the
--   session ID with a 2-byte length, then tls's 'encodeSessionData'.
encodeTLSResumption :: SessionID -> SessionData -> ByteString
encodeTLSResumption sid sd =
    BS.concat [BS.pack [0, fromIntegral (len `shiftR` 8), fromIntegral (len .&. 0xff)], sid, encodeSessionData sd]
  where
    len = BS.length sid

-- | The inverse of 'encodeTLSResumption'; 'Nothing' for anything else,
--   such as what an older version stored.
decodeTLSResumption :: ByteString -> Maybe (SessionID, SessionData)
decodeTLSResumption bs = case BS.unpack (BS.take 3 bs) of
    [0, h, l] -> do
        let len = fromIntegral h `shiftL` 8 .|. fromIntegral l
            (sid, rest) = BS.splitAt len $ BS.drop 3 bs
        if BS.length sid /= len
            then Nothing
            else (,) sid <$> decodeSessionData rest
    _ -> Nothing

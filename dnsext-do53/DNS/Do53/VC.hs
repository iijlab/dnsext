{-# LANGUAGE RecordWildCards #-}

module DNS.Do53.VC (
    vcPersistentResolver,
    tcpPersistentResolver,
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Tuple (swap)
import Network.Socket
import System.Timeout (timeout)

-- import System.IO.Error (annotateIOError)
-- import qualified DNS.Log as Log

import DNS.Do53.Do53 hiding (vcResolver)
import DNS.Do53.IO
import DNS.Do53.Imports
import DNS.Do53.Query
import DNS.Do53.Types
import qualified DNS.ThreadAsync as TAsync
import DNS.Types
import DNS.Types.Decode

type RVar = MVar (Either DNSError Reply)

-- | Persistent resolver over TCP.
tcpPersistentResolver :: PersistentResolver
tcpPersistentResolver ri@ResolveInfo{..} body = E.bracket open close $ \sock -> do
    let send = sendVC $ sendTCP sock
    -- One reader for the connection, not one for each message: see
    -- 'makeRecvVC'.
    recv <- makeRecvVC rinfoVCLimit $ recvTCP sock
    vcPersistentResolver tag send recv ri body
  where
    tag = nameTag ri "TCP"
    open = openTCP rinfoIP rinfoPort

-- | Making a persistent resolver.
vcPersistentResolver :: NameTag -> (BS -> IO ()) -> IO BS -> PersistentResolver
vcPersistentResolver tag send recv ResolveInfo{..} body = do
    inpQ <- newTQueueIO
    ref <- newIORef emp
    TAsync.race_
        "vcPersistentResolver: sender/receiver"
        (TAsync.concurrently_ "vcPersistentResolver:sender" (sender inpQ) "vcPersistentResolver:receiver" (recver ref))
        "vcPersistentResolver: body"
        (body $ resolve inpQ ref)
  where
    emp = IM.empty :: IntMap RVar
    resolve inpQ ref q qctl = do
        ident <- ractionGenId rinfoActions
        var <- newEmptyMVar :: IO RVar
        let key = fromIntegral ident
            qry = encodeQuery ident q qctl
            tx = BS.length qry
        atomicModifyIORef' ref (\m -> (IM.insert key var m, ()))
        atomically $ writeTQueue inpQ qry
        mres <- timeout (ractionTimeoutTime rinfoActions) $ takeMVar var
        return $ case mres of
            Nothing -> Left TimeoutExpired
            Just (Left e) -> Left e
            Just (Right rp) -> case checkRespM q ident (replyDNSMessage rp) of
                Nothing -> Right $ rp{replyTxBytes = tx}
                Just err -> Left err

    sender inpQ = forever (atomically (readTQueue inpQ) >>= send)

    del idnt m = swap $ IM.updateLookupWithKey (\_ _ -> Nothing) idnt m

    recver ref = forever $ do
        bs <-
            recv `E.catch` \ne -> do
                let e = fromIOException (fromNameTag tag) ne
                cleanup ref e
                E.throwIO e
        now <- ractionGetTime rinfoActions
        case decodeAt now bs of
            Left e -> do
                cleanup ref e
                E.throwIO e
            Right msg -> do
                let key = fromIntegral $ identifier msg
                Just var <- atomicModifyIORef' ref $ del key
                putMVar var $ Right $ Reply tag msg 0 {- dummy -} $ BS.length bs

    cleanup ref e = do
        vars <- IM.elems <$> readIORef ref
        mapM_ (\var -> putMVar var (Left e)) vars

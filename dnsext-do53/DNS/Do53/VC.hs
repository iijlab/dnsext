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
import qualified Network.Socket.ByteString as NSB

-- import System.IO.Error (annotateIOError)
import qualified DNS.Log as Log

import DNS.Do53.Do53 hiding (vcResolver)
import DNS.Do53.IO
import DNS.Do53.Imports
import DNS.Do53.Query
import DNS.Do53.Types
import qualified DNS.ThreadAsync as TAsync
import DNS.Types
import DNS.Types.Decode

type RVar = TMVar (Either DNSError Reply)

-- | Persistent resolver over TCP.
tcpPersistentResolver :: PersistentResolver
tcpPersistentResolver ri@ResolveInfo{..} body = E.bracket open close $ \sock -> do
    let send g = sendVC $ \bss -> orGaveUp $ NSB.sendManyWith sock bss g
    -- One reader for the connection, not one for each message: see
    -- 'makeRecvVC'.  What it gives up on is what the reader asks with.
    giveUpVar <- newTVarIO retry
    recv' <- makeRecvVC rinfoVCLimit $ do
        g <- readTVarIO giveUpVar
        orGaveUp $ NSB.recvWith sock 2048 g
    let recv g = (atomically (writeTVar giveUpVar g) >> recv') `E.finally` atomically (writeTVar giveUpVar retry)
    vcPersistentResolver tag send recv ri body
  where
    tag = nameTag ri "TCP"
    -- Connecting gives up at the query's deadline, as a query does.
    open =
        withDeadline (ractionTimeoutTime rinfoActions) (\g -> orGaveUp $ openTCPWith rinfoIP rinfoPort g)
            >>= maybe (E.throwIO TimeoutExpired) return

-- | Making a persistent resolver.  Sending and receiving give up on the
--   'STM' action with 'GaveUp': once the body is done, the sender and the
--   receiver are told so and end by themselves.
vcPersistentResolver
    :: NameTag -> (STM () -> BS -> IO ()) -> (STM () -> IO BS) -> PersistentResolver
vcPersistentResolver tag send recv ResolveInfo{..} body = do
    inpQ <- newTQueueIO
    ref <- newIORef emp
    done <- newTVarIO False
    let stopped = readTVar done >>= check
        workers =
            TAsync.concurrently_
                "vcPersistentResolver:sender"
                (sender stopped inpQ)
                "vcPersistentResolver:receiver"
                (recver stopped ref)
        -- Returning once the workers are, which the race waits for.
        body' = do
            body (resolve inpQ ref) `E.finally` atomically (writeTVar done True)
            forever $ threadDelay 1000000000
    TAsync.race_ "vcPersistentResolver: sender/receiver" workers "vcPersistentResolver: body" body'
  where
    emp = IM.empty :: IntMap RVar
    resolve inpQ ref q qctl = do
        ident <- ractionGenId rinfoActions
        var <- newEmptyTMVarIO :: IO RVar
        let key = fromIntegral ident
            qry = encodeQuery ident q qctl
            tx = BS.length qry
        atomicModifyIORef' ref (\m -> (IM.insert key var m, ()))
        atomically $ writeTQueue inpQ qry
        late <- registerDelay $ ractionTimeoutTime rinfoActions
        mres <- atomically $ (Just <$> takeTMVar var) `orElse` (Nothing <$ (readTVar late >>= check))
        -- Nobody will take this one now, and what is left in the table
        -- is what the answers are matched against.
        when (isNothing mres) $ atomicModifyIORef' ref (\m -> (IM.delete key m, ()))
        return $ case mres of
            Nothing -> Left TimeoutExpired
            Just (Left e) -> Left e
            Just (Right rp) -> case checkRespM q ident (replyDNSMessage rp) of
                Nothing -> Right $ rp{replyTxBytes = tx}
                Just err -> Left err

    sender stopped inpQ = ignoreGaveUp loop
      where
        loop = do
            mq <- atomically $ (Just <$> readTQueue inpQ) `orElse` (Nothing <$ stopped)
            forM_ mq $ \qry -> send stopped qry >> loop

    del idnt m = swap $ IM.updateLookupWithKey (\_ _ -> Nothing) idnt m

    recver stopped ref = ignoreGaveUp $ forever $ do
        bs <-
            recv stopped `E.catch` \ne -> do
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
                mvar <- atomicModifyIORef' ref $ del key
                case mvar of
                    Just var -> atomically $ putTMVar var $ Right $ Reply tag msg 0 {- dummy -} $ BS.length bs
                    -- An answer nobody is waiting for: one which came
                    -- twice, or one which came after its query gave up.
                    -- It used to be a pattern match failure here, which
                    -- took this thread and the whole connection with it.
                    Nothing ->
                        ractionLog
                            rinfoActions
                            Log.DEBUG
                            Nothing
                            ["vcPersistentResolver: an answer for ", show (identifier msg), " which nobody is waiting for"]

    cleanup ref e = do
        vars <- IM.elems <$> readIORef ref
        mapM_ (\var -> atomically $ putTMVar var (Left e)) vars

    ignoreGaveUp = E.handle $ \GaveUp -> return ()

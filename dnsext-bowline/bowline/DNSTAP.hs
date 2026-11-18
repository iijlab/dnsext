{-# LANGUAGE RecordWildCards #-}

module DNSTAP (
    DnstapQ,
    new,
    Message,
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import qualified DNS.Log as Log
import qualified DNS.TAP.FastStream as FSTRM
import DNS.TAP.Schema
import qualified DNS.ThreadStats as TStat
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket

import Config

-- | The action starting the writer returns the action stopping it.
new :: Config -> Log.PutLines IO -> IO (IO (IO ()), Message -> IO ())
new conf@Config{..} logP
    | cnf_dnstap = do
        (writer, put, stop) <- newDnstapWriter conf logP
        return (start writer stop, put)
    | otherwise = do
        let put ~_ = return ()
        return (return $ return (), put)
  where
    -- Killed only if it has not ended a second after being told to stop.
    start :: IO () -> IO () -> IO (IO ())
    start writer stop = do
        done <- newEmptyTMVarIO
        tid <- TStat.forkIO "bw.dnstap-writer" $ writer `E.finally` atomically (putTMVar done ())
        return $ do
            stop
            late <- registerDelay 1000000
            ended <- atomically $ (True <$ readTMVar done) `orElse` (False <$ (readTVar late >>= check))
            when (not ended) $ killThread tid

newtype DnstapQ = DnstapQ (TBQueue Message)

newDnstapQ :: IO DnstapQ
newDnstapQ = DnstapQ <$> newTBQueueIO queueBound
  where
    {- limit waiting area on server to constant size -}
    {- transactions per 1 millisecond. When under load, assume GC runs about every 1 millisecond and the thread switches -}
    queueBound = 64

writeDnstapQ :: DnstapQ -> Message -> IO ()
writeDnstapQ (DnstapQ q) ~msg = atomically $ writeTBQueue q msg

-- | 'Nothing' once stopped and the queue is empty.
readDnsTapQ :: DnstapQ -> TVar Bool -> IO (Maybe Message)
readDnsTapQ (DnstapQ q) stopped =
    atomically $ (Just <$> readTBQueue q) `orElse` (Nothing <$ (readTVar stopped >>= check))

newDnstapWriter :: Config -> Log.PutLines IO -> IO (IO (), Message -> IO (), IO ())
newDnstapWriter conf logP = do
    q <- newDnstapQ
    ref <- newIORef False
    stopped <- newTVarIO False
    let logger = control conf stopped $ exec conf q stopped ref logP
        put ~x = do
            ready <- readIORef ref
            when ready $ writeDnstapQ q x
    return (logger, put, atomically $ writeTVar stopped True)

exec :: Config -> DnstapQ -> TVar Bool -> IORef Bool -> Log.PutLines IO -> IO ()
exec Config{..} q stopped ref logP = E.bracket setup teardown $ \sock -> do
    let fconf = FSTRM.Config True False
    -- An empty frame ends the writer.
    FSTRM.writer sock fconf $ maybe BS.empty encode <$> readDnsTapQ q stopped
  where
    encode msg = encodeDnstap defaultDNSTAP{dnstapMessage = Just msg}
    logLn level = logP level Nothing . (: [])
    setup = do
        s <- E.bracketOnError open close conn
        writeIORef ref True
        return s
      where
        open = socket AF_UNIX Stream defaultProtocol
        conn sock = do
            connect sock $ SockAddrUnix cnf_dnstap_socket_path
            logLn Log.INFO ("DNSTAP connected: " ++ cnf_dnstap_socket_path)
            return sock
    teardown s = do
        writeIORef ref False
        logLn Log.INFO ("DNSTAP disconnected: " ++ cnf_dnstap_socket_path)
        close s

control :: Config -> TVar Bool -> IO () -> IO ()
control Config{..} stopped body = loop
  where
    loop = do
        ex <- E.try body
        case ex of
            Right () -> return ()
            -- SomeException: asynchronous exceptions are re-thrown
            Left se@(E.SomeException _)
                | Just (E.SomeAsyncException _) <- E.fromException se ->
                    E.throwIO se
                | otherwise -> do
                    late <- registerDelay (cnf_dnstap_reconnect_interval * 1000000)
                    stop <- atomically $ (False <$ (readTVar late >>= check)) `orElse` (readTVar stopped >>= check >> return True)
                    when (not stop) loop

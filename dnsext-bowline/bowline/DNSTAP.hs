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
import Data.IORef
import Network.Socket

import Config

new :: Config -> Log.PutLines IO -> IO (IO (Maybe ThreadId), Message -> IO ())
new conf@Config{..} logP
    | cnf_dnstap = do
        (writer, put) <- newDnstapWriter conf logP
        return (Just <$> TStat.forkIO "bw.dnstap-writer" writer, put)
    | otherwise = do
        let put ~_ = return ()
        return (return Nothing, put)

newtype DnstapQ = DnstapQ (TBQueue Message)

newDnstapQ :: IO DnstapQ
newDnstapQ = DnstapQ <$> newTBQueueIO queueBound
  where
    {- limit waiting area on server to constant size -}
    {- transactions per 1 millisecond. When under load, assume GC runs about every 1 millisecond and the thread switches -}
    queueBound = 64

writeDnstapQ :: DnstapQ -> Message -> IO ()
writeDnstapQ (DnstapQ q) ~msg = atomically $ writeTBQueue q msg

readDnsTapQ :: DnstapQ -> IO Message
readDnsTapQ (DnstapQ q) = atomically $ readTBQueue q

newDnstapWriter :: Config -> Log.PutLines IO -> IO (IO (), Message -> IO ())
newDnstapWriter conf logP = do
    q <- newDnstapQ
    ref <- newIORef False
    let logger = control conf $ exec conf q ref logP
        put ~x = do
            ready <- readIORef ref
            when ready $ writeDnstapQ q x
    return (logger, put)

exec :: Config -> DnstapQ -> IORef Bool -> Log.PutLines IO -> IO ()
exec Config{..} q ref logP = E.bracket setup teardown $ \sock -> do
    let fconf = FSTRM.Config True False
    FSTRM.writer sock fconf $ do
        msg <- readDnsTapQ q
        let d = defaultDNSTAP{dnstapMessage = Just msg}
        return $ encodeDnstap d
  where
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

control :: Config -> IO () -> IO ()
control Config{..} body = loop
  where
    loop = do
        ex <- E.try body
        case ex of
            Right () -> return ()
            Left se@(E.SomeException _)
                | Just (E.SomeAsyncException _) <- E.fromException se ->
                    E.throwIO se
                | otherwise -> do
                    threadDelay (cnf_dnstap_reconnect_interval * 1000000)
                    loop

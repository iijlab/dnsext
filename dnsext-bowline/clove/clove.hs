{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (concurrently_)
import qualified Control.Exception as E
import Control.Monad
import DNS.Do53.Internal
import Network.Run.TCP.Timeout
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Directory
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), IOMode (AppendMode), hClose, hPutStrLn, hSetBuffering, openFile, stderr)
import System.IO.Error (ioeGetErrorString, isUserError)
import System.Posix (Handler (Catch), installHandler, sigHUP)

import DNS.Auth.Algorithm
import DNS.Log
import qualified DNS.SEC as DNS
import qualified DNS.SVCB as DNS
import DNS.Types
import qualified DNS.Types as DNS
import Data.IORef

import qualified Auth
import Config
import Exception
import KeyFile
import Net
import Notify
import Types
import Zone

----------------------------------------------------------------

main :: IO ()
main = reportingError $ do
    DNS.runInitIO $ do
        DNS.addResourceDataForDNSSEC
        DNS.addResourceDataForSVCB
    -- Initialization
    conffile <- getConfFile
    (Config{..}, zonelist) <- loadConfig conffile
    --
    setCurrentDirectory cnf_clove_dir
    --
    withLogger Config{..} $ \env reopenLog -> do
        zones <- newZones env zonelist
        zoneAlist <- toZoneAlist zones
        -- Notify
        let (_, zonerefs) = unzip zoneAlist
        _ <- forkIO $ do
            threadDelay 1000000
            mapM_ (notifyWithZone env) zonerefs
        -- Zone updators.  SIGHUP also reopens the log file, as a
        -- daemon is expected to, so that log rotation does not leave us
        -- writing into a file nobody can find any more.
        let onHUP = sequence_ (map zoneWakeUp zones) >> reopenLog
        void $ installHandler sigHUP (Catch onHUP) Nothing
        mapM_ (void . forkIO . syncZone env) zonerefs
        -- AXFR servers: TCP
        let as = map (tcpServer env zoneAlist (show cnf_tcp_port)) cnf_tcp_addrs
        -- Authoritative servers: UDP
        ss <- mapM (serverSocket cnf_udp_port) cnf_udp_addrs
        let cs = map (udpServer env zoneAlist) ss
        -- Run servers
        case as ++ cs of
            [] -> die "no address to listen on: set tcp-addrs and/or udp-addrs"
            servers -> foldr1 concurrently_ servers

----------------------------------------------------------------

getConfFile :: IO FilePath
getConfFile = do
    args <- getArgs
    case args of
        [conffile] -> return conffile
        _ -> do
            name <- getProgName
            die $ "usage: " ++ name ++ " <config file>"

-- | Reporting a failure as one line rather than as an uncaught
--   exception with a backtrace.  This covers the whole of 'main', not
--   just the reading of the configuration file: a missing clove-dir, an
--   unusable log-file and a zone we cannot make sense of are all
--   reasons not to start, and each of them deserves a sentence rather
--   than a stack trace.  Anything which is not an 'IOError' is a bug in
--   clove and keeps the backtrace it deserves.
reportingError :: IO a -> IO a
reportingError action =
    action `E.catch` \e ->
        -- Our own complaints read better without the "user error"
        -- wrapper; a system error keeps its file name and its cause.
        die $ if isUserError e then ioeGetErrorString e else show e

die :: String -> IO a
die msg = do
    name <- getProgName
    hPutStrLn stderr $ name ++ ": " ++ msg
    exitFailure

----------------------------------------------------------------

-- | Setting up logging as the configuration asks for it.  "log: no"
--   means no logging at all, and "log-file" means a file rather than
--   standard output; both used to be read and then ignored.
--
--   The body is also handed the action which reopens the log, for the
--   signal handler to call.
withLogger :: Config -> (Env -> IO () -> IO a) -> IO a
withLogger Config{..} body
    | not cnf_log = body Env{envPutLines = \_ _ _ -> return ()} (return ())
    | otherwise = case cnf_log_file of
        Nothing -> withStdLogger name Stdout level toEnv
        Just file -> withHandleLogger name (pure id) (open file) hClose level toEnv
  where
    name = "clove logger"
    level = cnf_log_level
    toEnv Ops{..} = body Env{envPutLines = putLines} reopenLogger
    open file = do
        h <- openFile file AppendMode
        hSetBuffering h LineBuffering
        return h

----------------------------------------------------------------

udpServer :: Env -> ZoneAlist -> Socket -> IO ()
udpServer env zoneAlist s = Auth.server env proto zoneAlist
  where
    proto =
        Proto
            { recvQuery = NSB.recvFrom s 2048
            , sendReply = \sa bs -> void $ NSB.sendTo s bs sa
            , allowAXFR = \_ _ _ -> return Nothing
            , protoName = "UDP"
            , recvErrorFatal = False
            , replyLimit = Just . udpReplyLimit
            }

-- | RFC 1035 Sec 4.2.1 limits a UDP message to 512 bytes.  RFC 6891
--   Sec 6.2.3 lets the requestor offer a larger buffer with EDNS0, and
--   a responder must not send more than what was offered.
udpReplyLimit :: DNSMessage -> Int
udpReplyLimit query = fromIntegral $ case ednsHeader query of
    EDNSheader edns -> maxUdpSize `min` (minUdpSize `max` ednsUdpSize edns)
    _ -> minUdpSize

----------------------------------------------------------------

tcpServer
    :: Env
    -> ZoneAlist
    -> ServiceName
    -> HostName
    -> IO ()
tcpServer env zoneAlist port addr =
    runTCPServer 10 (Just addr) port $
        \_tmgr _h s -> do
            let proto =
                    Proto
                        { recvQuery = do
                            bs <- recvVC (32 * 1024) $ recvTCP s
                            sa <- getPeerName s
                            return (bs, sa)
                        , sendReply = \_sa bs -> sendVC (sendTCP s) bs
                        , allowAXFR = Auth.tcpAllowAXFR
                        , protoName = "TCP"
                        , recvErrorFatal = True
                        , -- A two byte length prefix: nothing to truncate.
                          replyLimit = const Nothing
                        }
            Auth.server env proto zoneAlist

----------------------------------------------------------------

syncZone :: Env -> IORef Zone -> IO ()
syncZone env zoneref = loopLogErr env WARNING go
  where
    go = do
        Zone{..} <- readIORef zoneref
        let mtm
                -- Key rollover
                | Just signing <- zoneSigning = Just $ signingZSKRollover signing
                -- Source is from file. No timeout.
                | zoneFromFile = Nothing
                | not zoneReady = Just 10 -- retry
                | otherwise = Just $ fromIntegral $ soa_refresh $ dbRD_SOA zoneDB
        -- A signal or timeout breaks this wait.
        zoneTimeoutWait mtm
        case zoneSigning of
            Nothing -> return ()
            Just Signing{..} -> do
                rolloverZSK (zoneDirectory zoneName) signingZSKRollover signingZSKConfig
        -- reading zone source
        updateZone env zoneref
        -- notify
        notifyWithZone env zoneref

notifyWithZone :: Env -> IORef Zone -> IO ()
notifyWithZone env zoneref = do
    Zone{..} <- readIORef zoneref
    -- The name comes from the configuration, not from the database: the
    -- empty database of a zone which failed to load carries the root as
    -- its apex, and we would be notifying our secondaries about ".".
    when zoneReady $ mapM_ (notify env zoneName) zoneNotifyAddrs

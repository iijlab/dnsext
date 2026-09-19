{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
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
import qualified System.TimeManager as T
import System.Timeout (timeout)

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
import TSIGKeys
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
        keys <- loadTSIGKeys env cnf_tsig_file
        envPutLines env INFO Nothing [show (countTSIGKeys keys) ++ " TSIG key(s)"]
        zones <- newZones env keys zonelist
        zoneAlist <- toZoneAlist zones
        let (_, zonerefs) = unzip zoneAlist
        -- Zone updators.  Each loads its own zone, so a source which is
        -- slow to answer holds up neither the other zones nor the
        -- servers below.  SIGHUP also reopens the log file, as a
        -- daemon is expected to, so that log rotation does not leave us
        -- writing into a file nobody can find any more.
        let onHUP = sequence_ (map zoneWakeUp zones) >> reopenLog
        void $ installHandler sigHUP (Catch onHUP) Nothing
        mapM_ (void . forkIO . syncZone env) zonerefs
        -- AXFR servers: TCP
        let as = map (tcpServer env keys zoneAlist (show cnf_tcp_port) cnf_transfer_time_limit) cnf_tcp_addrs
        -- Authoritative servers: UDP
        ss <- mapM (serverSocket cnf_udp_port) cnf_udp_addrs
        let cs = map (udpServer env keys zoneAlist) ss
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

udpServer :: Env -> TSIGKeys -> ZoneAlist -> Socket -> IO ()
udpServer env keys zoneAlist s = Auth.server env keys proto zoneAlist
  where
    proto =
        Proto
            { recvQuery = NSB.recvFrom s 2048
            , sendReply = \sa bs -> void $ NSB.sendTo s bs sa
            , allowAXFR = \_ _ _ _ -> return TransferRefused
            , protoName = "UDP"
            , recvErrorFatal = False
            , replyLimit = Just . udpReplyLimit
            , duringTransfer = id
            }

-- | RFC 1035 Sec 4.2.1 limits a UDP message to 512 bytes.  RFC 6891
--   Sec 6.2.3 lets the requestor offer a larger buffer with EDNS0, and
--   a responder must not send more than what was offered.
udpReplyLimit :: DNSMessage -> Int
udpReplyLimit query = fromIntegral $ case ednsHeader query of
    EDNSheader edns -> maxUdpSize `min` (minUdpSize `max` ednsUdpSize edns)
    _ -> minUdpSize

----------------------------------------------------------------

-- | How long a TCP connection may sit idle before it is closed.
--
--   RFC 7766 Sec 6.2.3 asks for an idle period "on the order of
--   seconds" and for at least a few of them, so that a client can make
--   the SOA and the AXFR of one refresh on one connection -- which RFC
--   1035 Sec 4.2.2 asked for first, along with not closing a connection
--   until what was asked for has been answered.
--
--   Idle is the word.  The same value used to run from the moment the
--   connection was accepted, whatever was happening on it, because the
--   timeout handle the server is handed was never touched: a transfer to
--   a far end which read it slowly was cut in half, and a client asking
--   a question every few seconds was cut off in the middle of a
--   conversation.  Every message received and every message sent now
--   puts the deadline off.
--
--   Thirty seconds rather than the ten it was, which is also what BIND
--   uses.  A send which blocks because the far end is reading slowly
--   cannot say so until it completes, and a peer whose window updates
--   come in large steps can hold one send for a good many seconds; ten
--   was inside that range.
tcpIdleTimeout :: Int
tcpIdleTimeout = 30

-- | How long the far end is given to take what has already been written
--   to it, once clove is done with the connection, in milliseconds.
--
--   The kernel takes a message from us as soon as there is room in the
--   socket buffer, so a transfer to a peer which reads slowly is
--   finished being written while some hundreds of kilobytes of it are
--   still on their way.  Closing the connection then loses them.  This
--   is the grace 'gracefulClose' gives the peer to catch up before the
--   descriptor goes; the five seconds it would use by default is not
--   enough for a peer on a slow link, which is the only kind that needs
--   it.
tcpDrainTimeout :: Int
tcpDrainTimeout = 30 * 1000

-- | Handing a zone over, with the idle timeout of the connection out
--   of the way and a limit of its own in its place.
--
--   'tcpIdleTimeout' is how long to wait for the peer to say something.
--   A transfer is not a wait for the peer: it is us writing, and a
--   write which blocks cannot say that it is making progress until it
--   returns.  The kernel takes the first few hundred kilobytes at once
--   and then one send waits for the peer to make room, and a peer which
--   reads slowly does not announce the room it has freed until a good
--   part of its buffer is empty -- tens of seconds at a few kilobytes a
--   second, whatever the size of the zone.  Measured against clove: a
--   peer reading 8 kB a second was cut off after 850 kB of a 1.5 MB
--   zone, not because anything was lost but because the connection
--   looked idle for thirty seconds while a single send was blocked.
--
--   So the clock is stopped for as long as the zone is going out, and
--   what bounds the transfer instead is how long the whole of it may
--   take.  Something has to: a peer which asks for a zone and then
--   stops reading altogether would otherwise hold a thread for ever.
--   That is transfer-time-limit, an hour by default, which is long
--   enough for a zone of a few megabytes to reach a peer on a very poor
--   link and short enough to be a limit.
handingOver :: Int -> T.Handle -> IO () -> IO ()
handingOver limit alive body = do
    done <- E.bracket_ (T.pause alive) (T.resume alive) $ timeout (limit * 1000000) body
    case done of
        Just () -> pure ()
        Nothing ->
            E.throwIO $
                userError $
                    "transfer unfinished after transfer-time-limit of " ++ show limit ++ " seconds"

tcpServer
    :: Env
    -> TSIGKeys
    -> ZoneAlist
    -> ServiceName
    -> Int
    -> HostName
    -> IO ()
tcpServer env keys zoneAlist port limit addr =
    runTCPServerWithSettings settings tcpIdleTimeout (Just addr) port $
        \_tmgr alive s -> do
            let proto =
                    Proto
                        { recvQuery = do
                            bs <- recvVC (32 * 1024) $ recvTCP s
                            T.tickle alive
                            sa <- getPeerName s
                            return (bs, sa)
                        , sendReply = \_sa bs -> do
                            sendVC (sendTCP s) bs
                            T.tickle alive
                        , allowAXFR = Auth.tcpAllowAXFR
                        , protoName = "TCP"
                        , recvErrorFatal = True
                        , -- A two byte length prefix: nothing to truncate.
                          replyLimit = const Nothing
                        , duringTransfer = handingOver limit alive
                        }
            Auth.server env keys proto zoneAlist
  where
    settings = defaultServerSettings{settingsGracefulCloseTimeout = tcpDrainTimeout}

----------------------------------------------------------------

syncZone :: Env -> IORef Zone -> IO ()
syncZone env zoneref = do
    -- The name never changes, so the label for the log can be taken
    -- once rather than on every pass.
    label <- zoneLabel . zoneName <$> readIORef zoneref
    -- The zone is loaded before the first wait rather than in newZone,
    -- so that the servers can start listening straight away.
    load
    loopLogErrIn env WARNING label go
  where
    load = do
        updateZone env zoneref
        notifyWithZone env zoneref
    go = do
        Zone{..} <- readIORef zoneref
        let refresh
                -- Source is from file. No timeout.
                | zoneFromFile = Nothing
                | not zoneReady = Just 10 -- retry
                -- RFC 1035 Sec 3.3.13: what comes after an attempt which
                -- failed is the retry interval, which is the shorter of
                -- the two and is there to get the zone back sooner.
                | zoneFailing = Just $ fromIntegral $ soa_retry $ dbRD_SOA zoneDB
                | otherwise = Just $ fromIntegral $ soa_refresh $ dbRD_SOA zoneDB
            -- Key rollover.  A signed zone has to wake up for this even
            -- when its source never changes.
            rollover = signingZSKRollover <$> zoneSigning
            -- Whichever comes first.  Taking only the rollover left a
            -- signed zone checking its upstream once a week, and never
            -- reaching the retry above.
            mtm = case (refresh, rollover) of
                (Just a, Just b) -> Just $ min a b
                (Nothing, b) -> b
                (a, Nothing) -> a
        -- A signal or timeout breaks this wait.
        zoneTimeoutWait mtm
        -- A rollover which goes wrong must not stop the zone from
        -- being loaded: that is how it would recover.
        case zoneSigning of
            Nothing -> return ()
            Just Signing{..} ->
                handleLogErrIn env WARNING (zoneLabel zoneName) () $
                    rolloverZSK (zoneDirectory zoneName) signingZSKRollover signingZSKPreserve signingZSKConfig
        -- reading zone source, and telling the secondaries about it
        load

notifyWithZone :: Env -> IORef Zone -> IO ()
notifyWithZone env zoneref = do
    Zone{..} <- readIORef zoneref
    -- The name comes from the configuration, not from the database: the
    -- empty database of a zone which failed to load carries the root as
    -- its apex, and we would be notifying our secondaries about ".".
    when zoneReady $
        mapM_ (\ip -> notify env zoneNotifyKey zoneName ip zoneNotifyPort) zoneNotifyAddrs

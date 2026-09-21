{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import qualified Control.Exception as E
import Control.Monad
import DNS.Do53.Internal
import qualified Data.ByteString as BS
import Network.Run.TCP.Timeout
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Directory
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), IOMode (AppendMode), hClose, hPutStrLn, hSetBuffering, openFile, stderr)
import System.IO.Error (ioeGetErrorString, isFullError, isUserError)
import System.Posix (Handler (Catch), epochTime, installHandler, sigHUP)
import qualified System.TimeManager as T
import System.Timeout (timeout)

import DNS.Auth.Algorithm
import DNS.Auth.DB (ZoneCheck (..))
import DNS.Log
import qualified DNS.SEC as DNS
import qualified DNS.SVCB as DNS
import DNS.Types
import qualified DNS.Types as DNS
import DNS.Types.Time (EpochTime)
import Data.IORef
import Data.List (isPrefixOf)

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
    Options{..} <- getOptions
    (Config{..}, zonelist) <- loadConfig optConfFile
    --
    setCurrentDirectory cnf_clove_dir
    --
    withLogger Config{..} $ \env reopenLog -> do
        when optInsecure $
            envPutLines env WARN Nothing ["--insecure: serving what would otherwise be refused"]
        keys <- loadTSIGKeys env cnf_tsig_file
        envPutLines env INFO Nothing [show (countTSIGKeys keys) ++ " TSIG key(s)"]
        zones <- newZones (zoneCheckOf optInsecure) env keys zonelist
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
        -- Counted across every address clove listens on, not once per
        -- address: a limit which each listener kept for itself would
        -- be as many times the limit as there are addresses.
        clients <- newSlots cnf_tcp_clients
        transfers <- newSlots cnf_transfers_out
        seldom <- newSeldom
        let tcpConf =
                TCPConf
                    { tcpClientTimeout = cnf_tcp_client_timeout
                    , tcpTransferLimit = cnf_transfer_time_limit
                    , tcpClients = clients
                    , tcpTransfers = transfers
                    , tcpSeldom = seldom
                    }
            as = map (tcpServer env keys zoneAlist (show cnf_tcp_port) tcpConf) cnf_tcp_addrs
        -- Authoritative servers: UDP
        ss <- mapM (serverSocket cnf_udp_port) cnf_udp_addrs
        let cs = map (udpServer env keys zoneAlist) ss
        -- Run servers
        case as ++ cs of
            [] -> die "no address to listen on: set tcp-addrs and/or udp-addrs"
            servers -> foldr1 concurrently_ servers

----------------------------------------------------------------

-- | What the command line says.
data Options = Options
    { optInsecure :: Bool
    -- ^ Whether clove may serve what it would otherwise refuse.  It is
    --   for showing a resolver a zone which is wrong on purpose, so that
    --   what a resolver does with one can be found out, and it is a
    --   command line switch rather than a configuration item so that no
    --   configuration file can turn it on by itself.
    , optConfFile :: FilePath
    }

zoneCheckOf :: Bool -> ZoneCheck
zoneCheckOf insecure
    | insecure = Unchecked
    | otherwise = Checked

getOptions :: IO Options
getOptions = do
    args <- getArgs
    case args of
        ["--insecure", conffile] -> return $ Options True conffile
        [conffile, "--insecure"] -> return $ Options True conffile
        [conffile] | not ("-" `isPrefixOf` conffile) -> return $ Options False conffile
        _ -> do
            name <- getProgName
            die $ "usage: " ++ name ++ " [--insecure] <config file>"

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
            , -- A zone is never handed over on a datagram: allowAXFR
              -- above refuses every one of them.
              transferSlot = return $ Just $ return ()
            }

-- | RFC 1035 Sec 4.2.1 limits a UDP message to 512 bytes.  RFC 6891
--   Sec 6.2.3 lets the requestor offer a larger buffer with EDNS0, and
--   a responder must not send more than what was offered.
udpReplyLimit :: DNSMessage -> Int
udpReplyLimit query = fromIntegral $ case ednsHeader query of
    EDNSheader edns -> maxUdpSize `min` (minUdpSize `max` ednsUdpSize edns)
    _ -> minUdpSize

----------------------------------------------------------------

-- | Largest query clove will read off a connection, in octets.  A DNS
--   message over TCP can announce up to 65535, and nothing we answer
--   needs anywhere near that much asked of us.
tcpMessageLimit :: Int
tcpMessageLimit = 32 * 1024

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
--   'tcpClientTimeout' is how long to wait for the peer to say
--   something.
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

-- | What the configuration says about connections and the transfers
--   over them.
data TCPConf = TCPConf
    { tcpClientTimeout :: Int
    -- ^ tcp-client-timeout: how long a connection may say nothing
    --   before it is closed.
    --
    --   RFC 7766 Sec 6.2.3 asks for an idle period "on the order of
    --   seconds" and for at least a few of them, so that a client can
    --   make the SOA and the AXFR of one refresh on one connection --
    --   which RFC 1035 Sec 4.2.2 asked for first, along with not
    --   closing a connection until what was asked for has been
    --   answered.
    --
    --   Idle is the word.  The same value used to run from the moment
    --   the connection was accepted, whatever was happening on it,
    --   because the timeout handle the server is handed was never
    --   touched: a transfer to a far end which read it slowly was cut
    --   in half, and a client asking a question every few seconds was
    --   cut off in the middle of a conversation.  Every message
    --   received and every message sent now puts the deadline off.
    --
    --   Thirty seconds by default rather than the ten it was, which is
    --   also what BIND uses.  A send which blocks because the far end
    --   is reading slowly cannot say so until it completes, and a peer
    --   whose window updates come in large steps can hold one send for
    --   a good many seconds; ten was inside that range.
    , tcpTransferLimit :: Int
    -- ^ transfer-time-limit
    , tcpClients :: Slots
    -- ^ tcp-clients
    , tcpTransfers :: Slots
    -- ^ transfers-out
    , tcpSeldom :: Seldom
    -- ^ Holding back the complaint which repeats.
    }

tcpServer
    :: Env
    -> TSIGKeys
    -> ZoneAlist
    -> ServiceName
    -> TCPConf
    -> HostName
    -> IO ()
tcpServer env keys zoneAlist port TCPConf{..} addr =
    runTCPServerWithSettings settings tcpClientTimeout (Just addr) port $
        \_tmgr alive s -> withClientSlot env tcpClients s $ do
            -- What a read brought back beyond the message it was
            -- asked for.  It belongs to the next one and has to be
            -- kept, or a peer which sends its next query without
            -- waiting -- which RFC 7766 Sec 6.2.1.1 asks clients to do
            -- -- is never answered for it.
            ahead <- newIORef BS.empty
            let proto =
                    Proto
                        { recvQuery = do
                            rest <- readIORef ahead
                            (bs, rest') <- recvMessage tcpMessageLimit s rest
                            writeIORef ahead rest'
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
                        , duringTransfer = handingOver tcpTransferLimit alive
                        , transferSlot = takeSlot tcpTransfers
                        }
            Auth.server env keys proto zoneAlist
  where
    settings =
        defaultServerSettings
            { settingsGracefulCloseTimeout = tcpDrainTimeout
            , settingsOnException = mishap env tcpSeldom
            }

-- | A complaint made at most once in a while, with a count of the ones
--   it stood in for.  What it holds is when it was last made and how
--   many have been held back since.
newtype Seldom = Seldom (IORef (EpochTime, Int))

newSeldom :: IO Seldom
newSeldom = Seldom <$> newIORef (0, 0)

-- | Longest clove will go on saying nothing about something which is
--   still happening, in seconds.
seldomInterval :: EpochTime
seldomInterval = 60

seldomly :: Env -> Seldom -> (Int -> String) -> IO ()
seldomly env (Seldom ref) line = do
    now <- fromIntegral . fromEnum <$> epochTime
    msaid <- atomicModifyIORef' ref $ \(said, held) ->
        if now - said >= seldomInterval
            then ((now, 0), Just held)
            else ((said, held + 1), Nothing)
    case msaid of
        Nothing -> return ()
        Just held -> envPutLines env WARNING Nothing ["    " ++ line held]

-- | What the TCP server catches for us and would otherwise drop.
--
--   Running out of file descriptors is the one that matters.  accept
--   fails, network-run waits a tenth of a second and tries it again,
--   and clove goes on running while answering nobody over TCP.  Left
--   to the default hook, which does nothing, that is entirely silent.
--   It is also the one which repeats -- ten times a second for as long
--   as it lasts -- so it is said and then not said again for a minute,
--   with a count of the ones held back.
--
--   The others are an exception which escaped a connection's handler
--   and a failure to close a connection.  Neither is expected: what a
--   connection does wrong is dealt with and reported where it happens,
--   so anything arriving here is worth a line as it comes.
mishap :: Env -> Seldom -> Maybe SockAddr -> E.SomeException -> IO ()
mishap env seldom mpeer se = case E.fromException se of
    Just ioe
        | isFullError ioe ->
            seldomly env seldom $ \held ->
                "out of file descriptors, so a connection could not be taken"
                    ++ (if held > 0 then " (and " ++ show held ++ " more since the last of these)" else "")
    _ -> envPutLines env WARNING Nothing ["    " ++ about ++ show se]
  where
    about = maybe "" (\peer -> "connection " ++ show peer ++ ": ") mpeer

-- | Serving a connection if clove is not already holding as many as
--   tcp-clients allows, and dropping it at once if it is.
--
--   Dropped rather than left unaccepted: a listening socket cannot be
--   told to stop accepting without stopping altogether, so the choice
--   is to take the connection and end it.  The write side is shut down
--   first so that the peer hears immediately, rather than waiting out
--   the close grace of a connection which is not going to say
--   anything.
withClientSlot :: Env -> Slots -> Socket -> IO () -> IO ()
withClientSlot env clients s body = do
    got <- takeSlot clients
    case got of
        Just release -> body `E.finally` release
        Nothing -> do
            peer <- trySync $ getPeerName s
            envPutLines
                env
                WARNING
                Nothing
                [ "    connections are all taken, so one was dropped"
                    ++ either (const "") (\sa -> ", from " ++ show sa) peer
                ]
            void $ trySync $ shutdown s ShutdownSend

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

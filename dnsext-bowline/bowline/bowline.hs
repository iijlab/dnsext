{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- GHC
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Exception (bracket_, finally)
import Control.Monad
import Data.ByteString.Builder
import Data.Functor
import qualified Data.IORef as I
import Data.Int (Int64)
import Data.String (fromString)
import GHC.Stats
import System.Environment (getArgs, lookupEnv)
import System.IO (BufferMode (..), IOMode (AppendMode), hClose, hSetBuffering, openFile)
import System.IO.Error (tryIOError)
import System.Posix (Handler (Catch), UserID, getRealUserID, installHandler, setEffectiveGroupID, setEffectiveUserID, sigHUP)
import System.Timeout (timeout)
import Text.Printf (printf)

-- dnsext-* deps
import DNS.Iterative.Server as Server
import qualified DNS.Log as Log
import qualified DNS.RRCache as Cache
import qualified DNS.SEC as DNS
import DNS.SVCB (TYPE (..))
import qualified DNS.SVCB as DNS
import qualified DNS.ThreadAsync as TAsync
import qualified DNS.ThreadStats as TStat
import qualified DNS.Types as DNS
import DNS.Types.Internal (TYPE (..))
import Network.Socket hiding (close)
import qualified Network.TLS.SessionTicket as ST

-- this package
import Config
import qualified DNSTAP as TAP
import qualified Monitor as Mon
import Prometheus
import SocketUtil
import Types
import qualified WebAPI as API

----------------------------------------------------------------

help :: IO ()
help = putStrLn "bowline [<confFile>] [<conf-key>=<conf-value> ...]"

----------------------------------------------------------------

run :: UserID -> IO Config -> IO ()
run ruid readConfig = do
    TStat.setThreadLabel "bw.main"
    -- TimeCache uses Control.AutoUpdate which
    -- does not provide a way to kill the internal thread.
    tcache <- newTimeCache
    conf <- readConfig
    (ri, nkSuccess, nkFailure, kSuccess, kFailure) <- newReloadInfo $ Server.getTime tcache
    let rsuccess = \case Reload -> nkSuccess; KeepCache -> kSuccess
        rfailure = \case Reload -> nkFailure; KeepCache -> kFailure
    go tcache Nothing ri (\m -> m{reloadSuccess = rsuccess, reloadFailure = rfailure}) conf
  where
    go tcache mcache ri um conf = do
        mng <- um <$> newControl readConfig
        gcache <- maybe (getCache tcache conf) return mcache
        void $ installHandler sigHUP (Catch $ reloadCmd mng KeepCache () ()) Nothing -- reloading with cache on SIGHUP
        runConfig tcache gcache mng ri ruid conf
        ctl <- getCommandAndClear mng
        case ctl of
            Quit -> putStrLn "\nQuiting..." -- fixme
            Reload1 rconf -> do
                putStrLn "\nReloading..." -- fixme
                stopCache $ gcacheRRCacheOps gcache
                go tcache Nothing ri um rconf
            KeepCache1 rconf -> do
                putStrLn "\nReloading with the current cache..." -- fixme
                go tcache (Just gcache) ri um rconf

runConfig :: TimeCache -> GlobalCache -> Control -> [(String, IO Int64)] -> UserID -> Config -> IO ()
runConfig tcache gcache@GlobalCache{..} mng0 reloadInfo ruid conf@Config{..} = do
    -- Setup
    let tmout = timeout cnf_resolve_timeout
        check_for_v6_ns
            | cnf_disable_v6_ns = pure True
            | otherwise = do
                let disabled _ = putStrLn "cnf_disable_v6_ns is False, but disabling, because IPv6 is not supported." $> True
                foldAddrInfo disabled (\_ -> pure False) Datagram (Just "::") 53
        readTrustAnchors' ps ins = do
            unless (null ps) $ putStrLn $ "loading trust-anchor-file: " ++ unwords ps
            unless (null ins) $ putStrLn $ "insecure domains: " ++ show ins
            negateTrustAnchors ins <$> readTrustAnchors ps
        readRootHint' path = do
            putStrLn $ "loading root-hints: " ++ path
            readRootHint path
    disable_v6_ns <- check_for_v6_ns
    (runLogger, putLines, killLogger, reopenLog0) <- getLogger ruid conf tcache
    (runSSLKeyLogger, putSSLKeyLog, killSSLKeyLogger) <- getSSLKeyLogger ruid conf
    --
    let rootpriv = do
            (runWriter, putDNSTAP) <- TAP.new conf putLines
            trustAnchors <- readTrustAnchors' cnf_trust_anchor_file cnf_domain_insecures
            rootHint <- mapM readRootHint' cnf_root_hints
            let setOps = setRootHint rootHint . setRootAnchor trustAnchors . setRRCacheOps gcacheRRCacheOps . setTimeCache tcache
            chaosZones <- getIdentity conf <&> \id_ -> getChaosZones $ id_ ++ getVersion conf
            stubZones <- getStubZones cnf_stub_zones trustAnchors
            updateHistogram <- getUpdateHistogram $ putStrLn "response_time_seconds_sum is not supported for Int shorter than 64bit."
            env <-
                newEmptyEnv <&> \env0 ->
                    (setOps env0)
                        { shortLog_ = cnf_short_log
                        , logLines_ = putLines
                        , logDNSTAP_ = putDNSTAP
                        , disableV6NS_ = disable_v6_ns
                        , chaosZones_ = chaosZones
                        , localZones_ = getLocalZones cnf_local_zones
                        , stubZones_ = stubZones
                        , negativeTrustAnchors_ = getNegTrustAnchors cnf_domain_insecures
                        , maxNegativeTTL_ = cropMaxNegativeTTL cnf_cache_max_negative_ttl
                        , failureRcodeTTL_ = cropFailureRcodeTTL cnf_cache_failure_rcode_ttl
                        , maxQueryCount_ = cnf_max_global_quota
                        , udpLimit_ = fromIntegral cnf_udp_limit_size
                        , putSSLKeyLog_ = putSSLKeyLog
                        , reloadInfo_ = reloadInfo
                        , nsid_ = cnf_nsid
                        , updateHistogram_ = updateHistogram
                        , timeout_ = tmout
                        }
            --  filled env available
            sm <- ST.newSessionTicketManager ST.defaultConfig{ST.ticketLifetime = cnf_tls_session_ticket_lifetime}
            addrs <- mapM (bindServers cnf_dns_addrs) $ trans cnf_credentials sm
            (mas, monInfo) <- Mon.bindMonitor conf env
            masock <- API.bindAPI conf
            return (runWriter, env, addrs, mas, monInfo, masock)
    -- recover root-privilege to bind network-port and to access private-key on reloading
    (runWriter, env, addrs, mas, monInfo, masock) <- withRoot ruid conf rootpriv
    -- actions list for threads
    cacherStats <- Server.getWorkerStats cnf_cachers
    workerStats <- Server.getWorkerStats cnf_workers
    (cachers, workers, toCacher) <- Server.mkPipeline env cacherStats workerStats
    servers <- sequence [(n,sks,) <$> mkserv env toCacher sks | (n, mkserv, sks) <- addrs, not (null sks)]
    mng <- getControl env cacherStats workerStats mng0{reopenLog = reopenLog0}
    let srvInfo1 name sas = unwords $ (name ++ ":") : map show sas
        monitors srvInfo = Mon.monitors conf env mng gcache srvInfo mas monInfo
    monitor <- monitors <$> mapM (\(n, _mk, sks) -> srvInfo1 n <$> mapM getSocketName sks) addrs
    -- Run
    gcacheSetLogLn putLines
    tidW <- runWriter
    runLogger
    runSSLKeyLogger
    tidA <- mapM (TStat.forkIO "bw.webapi-srv" . API.run mng) masock
    let withNum name xs = zipWith (\i x -> (name ++ printf "%4d" i, x)) [1 :: Int ..] xs
    let concServer =
            conc
                [ TAsync.concurrentlyList_ (withNum "bw.cacher" cachers)
                , TAsync.concurrentlyList_ (withNum "bw.worker" workers)
                , TAsync.concurrentlyList_ [(n, as) | (n, _sks, ass) <- servers, as <- ass]
                ]
    {- Advisedly separating 'dumper' thread from Async thread-tree
       - Keep the 'dumper' thread alive until the end for debugging purposes
       - Not to be affected by issued `cancel` to thread-tree
       The 'dumper' thread separated by forkIO automatically terminates
       when the 'main' thread ends, so there's no need for cleanup.          -}
    sequence_ [TStat.forkIO "bw.dumper" (TStat.dumper $ putLines Log.SYSTEM Nothing) | cnf_threads_dumper]
    race_ concServer (conc monitor)
        -- Teardown
        `finally` do
            mapM_ maybeKill [tidA, tidW]
            killSSLKeyLogger
            killLogger
    threadDelay 500000 -- avoiding address already in use
  where
    maybeKill = maybe (return ()) killThread
    trans creds sm =
        [ (cnf_udp, "bw.udp-srv", udpServers udpconf, Datagram, cnf_udp_port)
        , (cnf_tcp, "bw.tcp-srv", tcpServers vcconf, Stream, cnf_tcp_port)
        , (cnf_h2c, "bw.h2c-srv", http2cServers vcconf, Stream, cnf_h2c_port)
        , (cnf_h2, "bw.h2-srv", http2Servers vcconf, Stream, cnf_h2_port)
        , (cnf_h3, "bw.h3-srv", http3Servers vcconf, Datagram, cnf_h3_port)
        , (cnf_tls, "bw.tls-srv", tlsServers vcconf, Stream, cnf_tls_port)
        , (cnf_quic, "bw.quic-srv", quicServers vcconf, Datagram, cnf_quic_port)
        ]
      where
        vcconf =
            VcServerConfig
                { vc_query_max_size = cnf_vc_query_max_size
                , vc_idle_timeout = cnf_vc_idle_timeout
                , vc_slowloris_size = cnf_vc_slowloris_size
                , vc_credentials = creds
                , vc_session_manager = sm
                , vc_early_data_size = cnf_early_data_size
                , vc_interface_automatic = cnf_interface_automatic
                }
    conc = mapConcurrently_ id
    udpconf =
        UdpServerConfig
            { udp_interface_automatic = cnf_interface_automatic
            }

main :: IO ()
main = do
    DNS.runInitIO $ do
        DNS.addResourceDataForDNSSEC
        DNS.addResourceDataForSVCB
    args <- getArgs
    ruid <- getRealUserID
    case args of
        [] -> run ruid (return defaultConfig)
        a : _
            | a `elem` ["-h", "-help", "--help"] -> help
        confFile : aargs -> run ruid (withRootConf ruid $ parseConfig confFile aargs)

----------------------------------------------------------------

bindServers
    :: [HostName]
    -> (Bool, String, a, SocketType, PortNumber)
    -> IO (String, a, [Socket])
bindServers _ (False, n, a, _, _) = return (n, a, [])
bindServers hosts (True, n, a, socktype, port) = do
    as <- ainfosSkipError putStrLn socktype port hosts
    (n,a,) <$> mapM openBind as
  where
    openBind ai@AddrInfo{addrAddress = sa} = withLocationIOE (show sa ++ "/" ++ n) $ do
        s <- openSocket ai
        setSocketOption s ReuseAddr 1
        when (addrFamily ai == AF_INET6) $ setSocketOption s IPv6Only 1
        withFdSocket s setCloseOnExecIfNeeded
        bind s sa
        when (addrSocketType ai == Stream) $ listen s 1024
        return s

----------------------------------------------------------------

getCache :: TimeCache -> Config -> IO GlobalCache
getCache tc Config{..} = do
    ref <- I.newIORef $ \_ _ _ -> return ()
    let memoLogLn msg = do
            putLines <- I.readIORef ref
            putLines Log.WARN Nothing [msg]
        cacheConf = RRCacheConf cnf_cache_size 1800 memoLogLn $ Server.getTime tc
    cacheOps <- newRRCacheOps cacheConf
    let setLog = I.writeIORef ref
    return $ GlobalCache cacheOps (getCacheControl cacheOps) setLog

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
getCacheControl :: RRCacheOps -> CacheControl
getCacheControl RRCacheOps{..} =
    emptyCacheControl
    { ccRemove = rmName, ccRemoveType = rmType, ccRemoveBogus = rmBogus, ccRemoveNegative = rmNeg, ccClear = clearCache }
  where
    rmName name     = mapM_ (rmType name) types
    rmType name ty  = removeCache (DNS.Question name ty DNS.IN)
    types = [A, AAAA, NS, SOA, CNAME, DNAME, MX, PTR, SRV, TYPE 35, SVCB, HTTPS]
    rmBogus   = filterCache (\_ _ hit _ -> Cache.hitCases1 (\_ -> True) notBogus hit)
    notBogus  = Cache.positiveCases (\_ -> True) (\_ -> False) (\_ _ -> True)
    rmNeg     = filterCache (\_ _ hit _ -> Cache.hitCases1 (\_ -> False) (\_ -> True) hit)
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
getLogger :: UserID -> Config -> TimeCache -> IO (IO (), Log.PutLines IO, IO (), IO ())
getLogger ruid conf@Config{..} TimeCache{..}
    | cnf_log = do
        let getpts
                | cnf_log_timestamp  = getTimeStr <&> (. (' ' :))
                | otherwise          = pure id
            result hreop a _ p k r = return (void $ TStat.forkIO "bw.logger" a, p, k, hreop r)
            lk open close fr = Log.with getpts open close cnf_log_level (result fr)
            handle   = lk (pure $ Log.stdHandle cnf_log_output)         (\_ -> pure ()) (\_ -> pure ())
            file fn  = lk (withRoot ruid conf $ openFile fn AppendMode)  hClose         id
        maybe handle file cnf_log_file
    | otherwise = do
        let p _ _ ~_ = return ()
            n = return ()
        return (return (), p, n, n)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getSSLKeyLogger :: UserID -> Config -> IO (IO (), String -> IO (), IO ())
getSSLKeyLogger ruid conf =
    maybe (pure nolog) logger =<< lookupEnv "BOWLINE_SSLKEYLOGFILE"
  where
    logger fn = either left pure =<< tryIOError (logger' fn)
    left e = putStrLn ("sslkey-logfile: logger open failed: " ++ show e) $> nolog
    logger' fn = Log.with (pure id) (open fn) hClose Log.INFO $
        \a _ p k _ -> pure (void $ TStat.forkIO "bw.sslkey" a, \s -> p Log.INFO Nothing [s], k)
    open fn = do
        fh <- withRoot ruid conf $ openFile fn AppendMode
        hSetBuffering fh LineBuffering
        putStrLn $ "sslkey-logfile: opened: " ++ fn
        pure fh
    nolog = (nop, \_ -> return (), nop)
    nop = return ()
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
getIdentity :: Config -> IO [(DNS.Domain, LocalZoneType, [RR])]
getIdentity Config{..}
    | cnf_hide_identity       = Server.identityRefuse
    | Just s <- cnf_identity  = Server.identityString s
    | otherwise             = case cnf_identity_option of
        []                 ->   Server.identityHash
        "refuse"  : _      ->   Server.identityRefuse
        "hash"    : _      ->   Server.identityHash
        "host"    : _      ->   Server.identityHost
        "string"  : s : _  ->   Server.identityString s
        _         : _      ->   Server.identityHash
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getVersion :: Config -> [(DNS.Domain, LocalZoneType, [RR])]
getVersion Config{..}
    | cnf_hide_version       = Server.versionRefuse
    | Just s <- cnf_version  = Server.versionString s
    | otherwise            = case cnf_version_option of
        []                 ->  Server.versionBlank
        "refuse"  : _      ->  Server.versionRefuse
        "blank"   : _      ->  Server.versionBlank
        "show"    : _      ->  Server.versionShow
        "string"  : s : _  ->  Server.versionString s
        _         : _      ->  Server.versionBlank
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

getControl :: Env -> [WorkerStatOP] -> [WorkerStatOP] -> Control -> IO Control
getControl env cstats wstats mng0 = do
    let ucacheQSize = return (0, 0 {- TODO: update ServerMonitor to drop -})
        mng =
            mng0
                { getStats = getStats' env ucacheQSize
                , getWStats = getWStats' cstats wstats
                }
    return mng

----------------------------------------------------------------

getStats' :: Env -> IO (Int, Int) -> IO Builder
getStats' env _ucacheQSize = do
    enabled <- getRTSStatsEnabled
    gc <-
        if enabled
            then fromRTSStats <$> getRTSStats
            else return mempty
    st <- Server.getStats env "bowline_"
    return (gc <> st)

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
getWStats' :: [WorkerStatOP] -> [WorkerStatOP] -> IO Builder
getWStats' cstats wstats =
    format
    <$> (map ("cachers:" ++) <$> Server.pprWorkerStats 0 cstats)
    <*> (map ("workers:" ++) <$> Server.pprWorkerStats 1 wstats)
 where
   format x y = fromString . unlines $ x ++ y
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | Checking if this process has the root privilege.
amIrootUser :: IO Bool
amIrootUser = (== 0) <$> getRealUserID

recoverRoot :: IO ()
recoverRoot = do
    setEffectiveUserID 0
    setEffectiveGroupID 0

-- | Setting user and group.
setGroupUser :: Config -> IO ()
setGroupUser Config{..} = do
    setEffectiveGroupID cnf_group
    setEffectiveUserID cnf_user

withRoot :: UserID -> Config -> IO a -> IO a
withRoot ruid conf act
    | ruid == 0 = bracket_ recoverRoot (setGroupUser conf) act
    | otherwise = act

withRootConf :: UserID -> IO Config -> IO Config
withRootConf ruid getConf
    | ruid == 0 = recoverRoot >> getConf >>= \conf -> setGroupUser conf $> conf
    | otherwise = getConf

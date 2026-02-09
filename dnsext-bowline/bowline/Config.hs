{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Config (
    Config (..),
    defaultConfig,
    parseConfig,
    showConfig,
) where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad.Trans.State (StateT (..), evalStateT)
import Data.Char (toUpper)
import Data.Functor
import Data.List
import Data.List.Split (splitOn)
import Data.String (fromString)
import Network.Socket (PortNumber)
import Network.TLS (Credentials (..), credentialLoadX509)
import System.IO.Error (ioeGetErrorString, ioeSetErrorString, tryIOError)
import System.Posix (GroupID, UserID, getGroupEntryForName, getUserEntryForName, groupID, userID)

import DNS.Iterative.Internal (Address, LocalZoneType (..))
import qualified DNS.Log as Log
import DNS.Parser hiding (Parser)
import DNS.Types (DNSError, Domain, OD_NSID (..), ResourceRecord (..), isSubDomainOf, maxUdpSize, minUdpSize)
import qualified DNS.Types.Opaque as Opaque
import DNS.ZoneFile (Context (cx_name, cx_zone), defaultContext, parseLineRR)

import Parser

data Config = Config
    { cnf_user :: UserID
    , cnf_group :: GroupID
    , cnf_log :: Bool
    , cnf_log_file :: Maybe FilePath
    , cnf_log_output :: Log.StdHandle
    , cnf_log_level :: Log.Level
    , cnf_log_timestamp :: Bool
    , cnf_short_log :: Bool
    , cnf_cert_file :: FilePath
    , cnf_key_file :: FilePath
    , cnf_credentials :: Credentials
    , cnf_trust_anchor_file :: [FilePath]
    , cnf_root_hints :: Maybe FilePath
    , cnf_cache_size :: Int
    , cnf_disable_v6_ns :: Bool
    , cnf_hide_identity :: Bool
    , cnf_identity :: Maybe String
    , cnf_identity_option :: [String]
    , cnf_hide_version :: Bool
    , cnf_version :: Maybe String
    , cnf_version_option :: [String]
    , cnf_local_zones :: [(Domain, LocalZoneType, [ResourceRecord])]
    , cnf_stub_zones :: [(Domain, [Domain], [Address])]
    , cnf_domain_insecures :: [Domain]
    , cnf_nsid :: Maybe OD_NSID
    , cnf_dns_addrs :: [String]
    , cnf_resolve_timeout :: Int
    , cnf_cachers :: Int
    , cnf_workers :: Int
    , cnf_max_global_quota :: Int
    , cnf_udp_limit_size :: Int
    , cnf_udp :: Bool
    , cnf_udp_port :: PortNumber
    , cnf_vc_query_max_size :: Int
    , cnf_vc_idle_timeout :: Int
    , cnf_vc_slowloris_size :: Int
    , cnf_tcp :: Bool
    , cnf_tcp_port :: PortNumber
    , cnf_tls :: Bool
    , cnf_tls_port :: PortNumber
    , cnf_tls_session_ticket_lifetime :: Int
    , cnf_quic :: Bool
    , cnf_quic_port :: PortNumber
    , cnf_h2c :: Bool
    , cnf_h2c_port :: PortNumber
    , cnf_h2 :: Bool
    , cnf_h2_port :: PortNumber
    , cnf_h3 :: Bool
    , cnf_h3_port :: PortNumber
    , cnf_early_data_size :: Int
    , cnf_monitor_port :: PortNumber
    , cnf_monitor_addrs :: [String]
    , cnf_monitor_stdio :: Bool
    , cnf_monitor_keep_interval :: Int
    , cnf_threads_dumper :: Bool
    , cnf_dnstap :: Bool
    , cnf_dnstap_socket_path :: FilePath
    , cnf_dnstap_reconnect_interval :: Int
    , cnf_webapi :: Bool
    , cnf_webapi_addr :: String
    , cnf_webapi_port :: PortNumber
    , cnf_cache_max_negative_ttl :: Int
    , cnf_cache_failure_rcode_ttl :: Int
    , cnf_interface_automatic :: Bool
    }
    deriving (Show)

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_user = 0
        , cnf_group = 0
        , cnf_log = True
        , cnf_log_file = Nothing
        , cnf_log_output = Log.Stdout
        , cnf_log_level = Log.WARN
        , cnf_log_timestamp = False
        , cnf_short_log = False
        , cnf_cert_file = "fullchain.pem"
        , cnf_key_file = "privkey.pem"
        , cnf_credentials = Credentials []
        , cnf_trust_anchor_file = []
        , cnf_root_hints = Nothing
        , cnf_cache_size = 2 * 1024
        , cnf_disable_v6_ns = False
        , cnf_hide_identity = False
        , cnf_identity = Nothing
        , cnf_identity_option = []
        , cnf_hide_version = False
        , cnf_version = Nothing
        , cnf_version_option = []
        , cnf_local_zones = []
        , cnf_stub_zones = []
        , cnf_domain_insecures = []
        , cnf_nsid = Nothing
        , cnf_dns_addrs = ["127.0.0.1", "::1"]
        , cnf_resolve_timeout = 10000000
        , cnf_cachers = 4
        , cnf_workers = 128
        , cnf_max_global_quota = 64
        , {- https://datatracker.ietf.org/doc/html/rfc9000#section-8.1
             Address Validation during Connection Establishment
               "Clients MUST ensure that UDP datagrams containing
                Initial packets have UDP payloads of at least 1200 bytes,
                adding PADDING frames as necessary." -}
          cnf_udp_limit_size = 1200
        , cnf_udp = True
        , cnf_udp_port = 53
        , cnf_vc_query_max_size = 2048
        , cnf_vc_idle_timeout = 30
        , cnf_vc_slowloris_size = 50
        , cnf_tcp = True
        , cnf_tcp_port = 53
        , cnf_tls = True
        , cnf_tls_port = 853
        , cnf_tls_session_ticket_lifetime = 7200
        , cnf_quic = True
        , cnf_quic_port = 853
        , cnf_h2c = True
        , cnf_h2c_port = 80
        , cnf_h2 = True
        , cnf_h2_port = 443
        , cnf_h3 = True
        , cnf_h3_port = 443
        , cnf_early_data_size = 4096
        , cnf_monitor_port = 10023
        , cnf_monitor_addrs = []
        , cnf_monitor_stdio = False
        , cnf_monitor_keep_interval = 300
        , cnf_threads_dumper = False
        , cnf_dnstap = True
        , cnf_dnstap_socket_path = "/tmp/bowline.sock"
        , cnf_dnstap_reconnect_interval = 10
        , cnf_webapi = True
        , cnf_webapi_addr = "127.0.0.1"
        , cnf_webapi_port = 8080
        , cnf_cache_max_negative_ttl = 3600
        , cnf_cache_failure_rcode_ttl = 180
        , cnf_interface_automatic = False
        }

----------------------------------------------------------------

showConfig :: Config -> [String]
showConfig conf = showConfig1 conf ++ showConfig2 conf

{- FOURMOLU_DISABLE -}
showConfig1 :: Config -> [String]
showConfig1 Config{..} =
    [ showAddrPort "Mointor" True        cnf_monitor_addrs  cnf_monitor_port
    , showAddrPort "WebAPI"  cnf_webapi  [cnf_webapi_addr]  cnf_webapi_port
    , showAddrPort "UDP"     cnf_udp     cnf_dns_addrs      cnf_udp_port
    , showAddrPort "TCP"     cnf_tcp     cnf_dns_addrs      cnf_tcp_port
    , showAddrPort "TLS"     cnf_tls     cnf_dns_addrs      cnf_tls_port
    , showAddrPort "QUIC"    cnf_quic    cnf_dns_addrs      cnf_quic_port
    , showAddrPort "H2C"     cnf_h2c     cnf_dns_addrs      cnf_h2c_port
    , showAddrPort "H2"      cnf_h2      cnf_dns_addrs      cnf_h2_port
    , showAddrPort "H3"      cnf_h3      cnf_dns_addrs      cnf_h3_port
    ]
  where
    showAddrPort tag enable addrs port
        | enable = tag ++ ": " ++ intercalate ", " (map (addrport port) addrs)
        | otherwise = tag ++ ": disabled"
    addrport port a
        | ':' `elem` a = "[" ++ a ++ "]:" ++ show port
        | otherwise = a ++ ":" ++ show port
{- FOURMOLU_ENABLE -}

showConfig2 :: Config -> [String]
showConfig2 conf =
    [ field' "user" cnf_user
    , field' "group" cnf_group
    , field' "log" cnf_log
    , field'_ "log file" (maybe "<no logfile>" id . cnf_log_file)
    , field'_ "log output" (showOut . cnf_log_output)
    , field' "log level" cnf_log_level
    , field' "log timestamp" cnf_log_timestamp
    , field' "short log" cnf_short_log
    , field'_ "cert file" cnf_cert_file
    , field'_ "key file" cnf_key_file
    , field'_ "trust anchor file" (unwords . cnf_trust_anchor_file)
    , field'_ "root hints" (maybe "<default>" id . cnf_root_hints)
    , field' "max cache size" cnf_cache_size
    , field' "disable queries to IPv6 NS" cnf_disable_v6_ns
    , field'_ "domain insecures" (unwords . map show . cnf_domain_insecures)
    , field'_ "dns addrs" (unwords . cnf_dns_addrs)
    , field' "resolve timeout" cnf_resolve_timeout
    , field' "cachers" cnf_cachers
    , field' "workers" cnf_workers
    , field' "max global quota" cnf_max_global_quota
    , field' "udp limit size" cnf_udp_limit_size
    , field' "udp" cnf_udp
    , field' "udp port" cnf_udp_port
    , field' "tls" cnf_tls
    , field' "tls port" cnf_tls_port
    , field' "tls session ticket lifetime" cnf_tls_session_ticket_lifetime
    , field' "quic" cnf_quic
    , field' "quic port" cnf_quic_port
    , field' "h2c" cnf_h2c
    , field' "h2c port" cnf_h2c_port
    , field' "h2" cnf_h2
    , field' "h2 port" cnf_h2_port
    , field' "h3" cnf_h3
    , field' "h3 port" cnf_h3_port
    , field' "early data size" cnf_early_data_size
    , field' "monitor port" cnf_monitor_port
    , field'_ "monitor addrs" (unwords . cnf_monitor_addrs)
    , field' "monitor stdio" cnf_monitor_stdio
    , field' "monitor keep interval" cnf_monitor_keep_interval
    , field' "threads dumpoer" cnf_threads_dumper
    , field' "dnstap" cnf_dnstap
    , field'_ "dnstap socket path" cnf_dnstap_socket_path
    , field' "dnstap reconnect interval" cnf_dnstap_reconnect_interval
    , field' "webapi" cnf_webapi
    , field'_ "webapi addr" cnf_webapi_addr
    , field' "webapi port" cnf_webapi_port
    , field' "cache max negative ttl" cnf_cache_max_negative_ttl
    , field' "cache failure rcode ttl" cnf_cache_failure_rcode_ttl
    , field' "interface automatic" cnf_interface_automatic
    ]
  where
    field' label' get = field'_ label' (show . get)
    field'_ label' toS = label' ++ ": " ++ toS conf
    showOut Log.Stdout = "stdout"
    showOut Log.Stderr = "stderr"

----------------------------------------------------------------

-- | Parsing a configuration file to get an 'Config'.
parseConfig :: FilePath -> [String] -> IO Config
parseConfig file args =
    makeConfig defaultConfig =<< loadNested nestedLimit =<< (++) <$> mapM readArg args <*> parseFile config file

{- FOURMOLU_DISABLE -}
makeConfig :: Config -> [Conf] -> IO Config
makeConfig def conf = do
    cnf_user <- get "user" cnf_user
    cnf_group <- get "group" cnf_group
    cnf_log <- get "log" cnf_log
    cnf_log_file <- get "log-file" cnf_log_file
    let cnf_log_output = Log.Stdout
    cnf_log_level <- get "log-level" cnf_log_level
    cnf_log_timestamp <- get "log-timestamp" cnf_log_timestamp
    cnf_short_log <- get "short-log" cnf_short_log
    cnf_cert_file <- get "cert-file" cnf_cert_file
    cnf_key_file <- get "key-file" cnf_key_file
    cnf_trust_anchor_file <- getTrustAnchorFile conf
    cnf_root_hints <- get "root-hints" cnf_root_hints
    cnf_cache_size <- get "cache-size" cnf_cache_size
    cnf_disable_v6_ns <- get "disable-v6-ns" cnf_disable_v6_ns
    cnf_hide_identity <- get "hide-identity" cnf_hide_identity
    cnf_identity <- get "identity" cnf_identity
    cnf_identity_option <- get "identity-option" cnf_identity_option
    cnf_hide_version <- get "hide-version" cnf_hide_version
    cnf_version <- get "version" cnf_version
    cnf_version_option <- get "version-option" cnf_version_option
    cnf_local_zones <- localZones
    cnf_stub_zones <- stubZones
    cnf_domain_insecures <- domainInsecures
    cnf_dns_addrs <- get "dns-addrs" cnf_dns_addrs
    cnf_nsid <- get "nsid" cnf_nsid
    cnf_resolve_timeout <- get "resolve-timeout" cnf_resolve_timeout
    cnf_cachers <- get "cachers" cnf_cachers
    cnf_workers <- get "workers" cnf_workers
    cnf_max_global_quota <- get "max-global-quota" cnf_max_global_quota
    let udpRange v = (fromIntegral minUdpSize) `max` (v `min` (fromIntegral maxUdpSize))
    cnf_udp_limit_size <- udpRange <$> get "udp-limit-size" cnf_udp_limit_size
    cnf_udp <- get "udp" cnf_udp
    cnf_udp_port <- get "udp-port" cnf_udp_port
    cnf_vc_query_max_size <- get "vc-query-max-size" cnf_vc_query_max_size
    cnf_vc_idle_timeout <- get "vc-idle-timeout" cnf_vc_idle_timeout
    cnf_vc_slowloris_size <- get "vc-slowloris-size" cnf_vc_slowloris_size
    cnf_tcp <- get "tcp" cnf_tcp
    cnf_tcp_port <- get "tcp-port" cnf_tcp_port
    cnf_tls <- get "tls" cnf_tls
    cnf_tls_port <- get "tls-port" cnf_tls_port
    cnf_tls_session_ticket_lifetime <- get "tls-session-ticket-lifetime" cnf_tls_session_ticket_lifetime
    cnf_quic <- get "quic" cnf_quic
    cnf_quic_port <- get "quic-port" cnf_quic_port
    cnf_h2c <- get "h2c" cnf_h2c
    cnf_h2c_port <- get "h2c-port" cnf_h2c_port
    cnf_h2 <- get "h2" cnf_h2
    cnf_h2_port <- get "h2-port" cnf_h2_port
    cnf_h3 <- get "h3" cnf_h3
    cnf_h3_port <- get "h3-port" cnf_h3_port
    cnf_early_data_size <- get "early_data_size" cnf_early_data_size
    cnf_monitor_port <- get "monitor-port" cnf_monitor_port
    cnf_monitor_addrs <- get "monitor-addrs" cnf_monitor_addrs
    cnf_monitor_stdio <- get "monitor-stdio" cnf_monitor_stdio
    cnf_monitor_keep_interval <- get "monitor-keep-interval" cnf_monitor_keep_interval
    cnf_threads_dumper <- get "threads-dumper" cnf_threads_dumper
    cnf_dnstap <- get "dnstap" cnf_dnstap
    cnf_dnstap_socket_path <- get "dnstap-socket-path" cnf_dnstap_socket_path
    cnf_dnstap_reconnect_interval <- get "dnstap-reconnect-interval" cnf_dnstap_reconnect_interval
    cnf_webapi <- get "webapi" cnf_webapi
    cnf_webapi_addr <- get "webapi-addr" cnf_webapi_addr
    cnf_webapi_port <- get "webapi-port" cnf_webapi_port
    cnf_cache_max_negative_ttl <- get "cache-max-negative-ttl" cnf_cache_max_negative_ttl
    cnf_cache_failure_rcode_ttl <- get "cache-failure-rcode-ttl" cnf_cache_failure_rcode_ttl
    cnf_interface_automatic <- get "interface-automatic" cnf_interface_automatic
    let getCreds
            | cnf_tls || cnf_quic || cnf_h2 || cnf_h3 = loadCredentials cnf_cert_file cnf_key_file
            | otherwise = pure $ Credentials []
    cnf_credentials <- getCreds
    pure Config{..}
  where
    get k func = do
        et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
        let left e = do
                let e' = ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
                ioError e'
        either left pure et
    --
    localZones = unfoldrM getLocalZone conf >>= \zs -> case mapM parseLocalZone zs of
        Right zones -> pure zones
        Left es -> fail $ "parse error during local-data: " ++ es
    parseLocalZone (d, zt, xs) = evalStateT ((,,) d zt . subdoms d <$> mapM getRR xs) defaultContext{cx_zone = d, cx_name = d}
    subdoms d rrs = [rr | rr <- rrs, rrname rr `isSubDomainOf` d]
    getRR s = StateT $ parseLineRR $ fromString s
    --
    stubZones = unfoldrM getStubZone conf
    --
    domainInsecures = unfoldrM getDomainInsecure conf
    --
    credLeft s = fail $ "config: fail to load credentials: " ++ s
    loadCredentials certf keyf = either credLeft (\c@(!_cc, !_pri) -> pure (Credentials [c])) =<< credentialLoadX509 certf keyf
{- FOURMOLU_ENABLE -}

-- $setup
-- >>> :seti -XOverloadedStrings

getTrustAnchorFile :: [Conf] -> IO [FilePath]
getTrustAnchorFile = mapM (fromConf . snd) . filter ((== "trust-anchor-file") . fst)

{- FOURMOLU_DISABLE -}
-- |
-- >>> getLocalZone [("foo",CV_Int 4),("local-zone",CV_Strings ["example.", "static"]),("local-data",CV_String "a.example. A 203.0.113.5"),("bar",CV_Bool True)]
-- Just (("example.",LZ_Static,["a.example. A 203.0.113.5"]),[("bar",CV_Bool True)])
getLocalZone :: [Conf] -> IO (Maybe ((Domain, LocalZoneType, [String]), [Conf]))
getLocalZone [] = pure Nothing
getLocalZone ((k, v):xs)
    | k == "local-zone" = do
          cstrs <- fromConf v
          let err = fail $ "unknown local-zone pattern: " ++ show cstrs
          (zone, zt) <- maybe err pure $ getLocalZone' cstrs
          (ds, ys) <- getLocalData id xs
          pure $ Just ((zone, zt, ds), ys)
    | otherwise = getLocalZone xs
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- |
-- >>> getLocalZone' ["example.", "static"]
-- Just ("example.",LZ_Static)
-- >>> getLocalZone' ["example.", "redirect"]
-- Just ("example.",LZ_Redirect)
getLocalZone' :: [String] -> Maybe (Domain, LocalZoneType)
getLocalZone' [s1,s2] = (,) (fromString s1) <$> zoneType s2
  where
    zoneType s = case s of
        "deny"      -> Just LZ_Deny
        "refuse"    -> Just LZ_Refuse
        "static"    -> Just LZ_Static
        "redirect"  -> Just LZ_Redirect
        _           -> Nothing
getLocalZone' _       = Nothing
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getLocalData :: ([String] -> [String]) -> [Conf] -> IO ([String], [Conf])
getLocalData a []        = pure (a [], [])
getLocalData a xxs@((k, v):xs)
    | k == "local-data"  = fromConf v >>= \vstr -> getLocalData (a . (vstr :)) xs
    | otherwise          = pure (a [], xxs)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getStubZone :: [Conf] -> IO (Maybe ((Domain, [Domain], [Address]), [Conf]))
getStubZone  []  = pure Nothing
getStubZone ((k, v):xs)
    | k == "stub-zone" = do
          apex <- fromString <$> fromConf v
          (ds, as, ys) <- getStubContent id id xs
          pure $ Just ((apex, ds, as), ys)
    | otherwise  = getStubZone xs
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getStubContent :: ([Domain] -> [Domain]) -> ([Address] -> [Address]) -> [Conf] -> IO ([Domain], [Address], [Conf])
getStubContent ds as      []  = pure (ds [], as [], [])
getStubContent ds as xss@((k, v):xs)
    | k == "stub-addr"         = do
        vstr <- fromConf v
        let (ip', port') = break (== '@') vstr
        ip <- read' "stub-zone: ip-address format error" ip'
        port <- case port' of
                    []   -> pure 53
                    _:p  -> read' "stub-zone: port format error" p
        getStubContent ds (as . ((ip, port) :)) xs
    | k == "stub-host"         = fromConf v >>= \vstr -> getStubContent (ds . (fromString vstr :)) as xs
    | otherwise                = pure (ds [], as [], xss)
  where
    read' e s = case [ x | (x, "") <- reads s ] of
        []   -> fail e
        x:_  -> pure x
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
getDomainInsecure :: [Conf] -> IO (Maybe (Domain, [Conf]))
getDomainInsecure  []         = pure Nothing
getDomainInsecure ((k, v):xs)
    | k == "domain-insecure"  = do
          vstr <- fromConf v
          either (left vstr) right =<< E.try (E.evaluate $ fromString vstr)
    | otherwise = getDomainInsecure xs
  where
    left :: String -> DNSError -> IO a
    left vstr e = fail ("domain-insecure: " ++ show e ++ ": " ++ vstr)
    right d = pure $ Just (d, xs)
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

type Conf = (String, ConfValue)

data ConfValue = CV_Int Int | CV_Bool Bool | CV_String String | CV_Strings [String] deriving (Eq, Show)

class FromConf a where
    fromConf :: ConfValue -> IO a

instance FromConf Int where
    fromConf (CV_Int n) = pure n
    fromConf cv = failWith cv "fromConf int"

instance FromConf PortNumber where
    fromConf (CV_Int n) = pure $ fromIntegral n
    fromConf cv = failWith cv "fromConf port"

instance FromConf Bool where
    fromConf (CV_Bool b) = pure b
    fromConf cv = failWith cv "fromConf bool"

instance FromConf String where
    fromConf (CV_String s) = pure s
    fromConf cv = failWith cv "fromConf string"

instance FromConf (Maybe String) where
    fromConf (CV_String "") = pure Nothing
    fromConf (CV_String s) = pure $ Just s
    fromConf cv = failWith cv "fromConf maybe string"

instance FromConf [String] where
    fromConf (CV_String s) = pure $ filter (/= "") $ splitOn "," s
    fromConf (CV_Strings ss) = pure ss
    fromConf cv = failWith cv "fromConf string list"

instance FromConf (Maybe OD_NSID) where
    fromConf (CV_String "") = pure Nothing
    fromConf (CV_String s) = Just <$> decodeNSID s
    fromConf cv = failWith cv "fromConf maybe NSID"

instance FromConf UserID where
    fromConf (CV_String s) = uidForName s
    fromConf (CV_Int i) = pure $ fromIntegral i
    fromConf cv = failWith cv "fromConf user-ID"

instance FromConf GroupID where
    fromConf (CV_String s) = gidForName s
    fromConf (CV_Int i) = pure $ fromIntegral i
    fromConf cv = failWith cv "fromConf group-ID"

instance FromConf Log.Level where
    fromConf (CV_String s) = logLevel s
    fromConf cv = failWith cv "fromConf log level"

failWith :: Show a => a -> String -> IO b
failWith x s = fail (s ++ ": " ++ show x)

decodeNSID :: String -> IO OD_NSID
decodeNSID s =
    maybe (fail "nsid: NSID must be hex-string or ascii-string with ascii_ prefix") (pure . OD_NSID) $ decodeAscii <|> decodeB16
  where
    decodeAscii = fromString <$> stripPrefix "ascii_" s
    decodeB16 = either (\_ -> Nothing) Just $ Opaque.fromBase16 $ fromString s

{- FOURMOLU_DISABLE -}
uidForName :: String -> IO UserID
uidForName s = either (nameError ("user: " ++ s)) (pure . userID) =<< tryIOError (getUserEntryForName s)

gidForName :: String -> IO GroupID
gidForName s = either (nameError ("group: " ++ s)) (pure . groupID) =<< tryIOError (getGroupEntryForName s)

nameError :: String -> IOError -> IO a
nameError n ioe = ioError $ ioeSetErrorString ioe n
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

logLevel :: String -> IO Log.Level
logLevel s = case lvs of
    lv : _ -> pure lv
    [] -> fail $ "fromConf unknwon log-level " ++ s
  where
    lvs = [lv | (lv, "") <- reads u]
    u = map toUpper s

----------------------------------------------------------------

loadNested :: Int -> [Conf] -> IO [Conf]
loadNested = expandNested loadInclude

loadInclude :: FilePath -> IO [Conf]
loadInclude path = do
    putStrLn $ "loading included conf: " ++ path
    parseFile config path

{- FOURMOLU_DISABLE -}
-- |
-- >>> include = pure . maybe [] id . (`lookup` [("n1", [("include", CV_String "n3")]), ("n2", [("bar", CV_Int 2)]), ("n3", [("baz", CV_Int 5)])])
-- >>> runNested n cs = expandNested include n cs
-- >>> runNested 0 []
-- *** Exception: ... limit exceeded...
-- >>> runNested 5 []
-- []
-- >>> runNested 1 [("include", CV_String "n1")]
-- *** Exception: ... limit exceeded...
-- >>> runNested 5 [("foo", CV_Int 3)]
-- [("foo",CV_Int 3)]
-- >>> runNested 5 [("foo", CV_Int 3), ("include", CV_String "n2")]
-- [("foo",CV_Int 3),("bar",CV_Int 2)]
-- >>> runNested 5 [("foo", CV_Int 3), ("include", CV_String "n2"), ("bar", CV_Int 7), ("include", CV_String "n1"), ("baz", CV_Int 6)]
-- [("foo",CV_Int 3),("bar",CV_Int 2),("bar",CV_Int 7),("baz",CV_Int 5),("baz",CV_Int 6)]
expandNested
    :: (FilePath -> IO [Conf])
    -> Int -> [Conf] -> IO [Conf]
expandNested _       n _
    | n <= 0              = fail $ "config: config-file nested-inclusion limit is " ++ show nestedLimit ++ ", limit exceeded."
expandNested loadInc n cs0 = include cs0
  where
    include  []           = pure []
    include (c@(k, v):cs)
        | k == "include"  = fromConf v >>= \path -> (++) <$> (expandNested loadInc (n - 1) =<< loadInc path) <*> include cs
        | otherwise       = chunk (c:) cs
    chunk ac      []      = pure (ac [])
    chunk ac ccs@(c@(k, _):cs)
        | k == "include"  = (ac [] ++) <$> include ccs
        | otherwise       = chunk (ac . (c:)) cs

nestedLimit :: Int
nestedLimit = 5
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

unfoldrM :: (b -> IO (Maybe (a, b))) -> b -> IO [a]
unfoldrM next = go id
  where
    go xs s = maybe (pure $ xs []) (\(x, s') -> go (xs . (x :)) s') =<< next s

----------------------------------------------------------------

readArg :: String -> IO Conf
readArg = parseString arg

----------------------------------------------------------------

config :: MonadParser W8 s m => m [Conf]
config = commentLines *> many cfield <* eof
  where
    cfield = field <* commentLines

-- |
-- >>> parse field "" "int: 3\n"
-- Right ("int",CV_Int 3)
-- >>> parse field "" "bool: yes\n"
-- Right ("bool",CV_Bool True)
-- >>> parse field "" "str: foo\n"
-- Right ("str",CV_String "foo")
-- >>> parse field "" "prefix-int: 127.0.0.1,::1 # comment \n"
-- Right ("prefix-int",CV_String "127.0.0.1,::1")
-- >>> parse field "" "prefix-bool-1: nothing # comment \n"
-- Right ("prefix-bool-1",CV_String "nothing")
-- >>> parse field "" "prefix-bool-2: yesterday # comment \n"
-- Right ("prefix-bool-2",CV_String "yesterday")
-- >>> parse field "" "d-quoted: \"foo bar baz\" # foo\n"
-- Right ("d-quoted",CV_String "foo bar baz")
-- >>> parse field "" "s-quoted: 'foo bar baz' # foo\n"
-- Right ("s-quoted",CV_String "foo bar baz")
-- >>> parse field "" "list: \"a b\" c\n"
-- Right ("list",CV_Strings ["a b","c"])
-- >>> parse field "" "listc: \"d e\" f # comment \n"
-- Right ("listc",CV_Strings ["d e","f"])
field :: MonadParser W8 s m => m Conf
field = (,) <$> key <*> (sep *> value) <* trailing

arg :: MonadParser W8 s m => m Conf
arg = (,) <$> key <*> (char '=' *> value)

key :: MonadParser W8 s m => m String
key = some (oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-") <* spcs

sep :: MonadParser W8 s m => m ()
sep = void $ char ':' *> spcs

squote :: MonadParser W8 s m => m ()
squote = void $ char '\''

dquote :: MonadParser W8 s m => m ()
dquote = void $ char '"'

value :: MonadParser W8 s m => m ConfValue
value = choice [cv_int, cv_bool, cv_strings]

eov :: MonadParser W8 s m => m ()
eov = void (lookAhead $ choice [char '#', char ' ', char '\n']) <|> eof

-- Trailing should be included in try to allow IP addresses.
cv_int :: MonadParser W8 s m => m ConfValue
cv_int = CV_Int . read <$> some digit <* eov

{- FOURMOLU_DISABLE -}
cv_bool :: MonadParser W8 s m => m ConfValue
cv_bool =
    CV_Bool True <$ string "yes" <* eov <|>
    CV_Bool False <$ string "no" <* eov

cv_string' :: MonadParser W8 s m => m String
cv_string' =
    squote *> many (noneOf "'\n")  <* squote <|>
    dquote *> many (noneOf "\"\n") <* dquote <|>
    some (noneOf "\"# \t\n")
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- |
-- >>> parse cv_strings "" "\"conf.txt\""
-- Right (CV_String "conf.txt")
-- >>> parse cv_strings "" "\"example. 1800 TXT 'abc'\" static"
-- Right (CV_Strings ["example. 1800 TXT 'abc'","static"])
cv_strings :: MonadParser W8 s m => m ConfValue
cv_strings = do
    v1 <- cv_string'
    vs <- many (spcs1 *> cv_string')
    pure $ if null vs
           then CV_String v1
           else CV_Strings $ v1:vs
{- FOURMOLU_ENABLE -}

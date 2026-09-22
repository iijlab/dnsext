{-# LANGUAGE RecordWildCards #-}

module Config (
    Config (..),
    loadConfig,
    ZoneConf (..),

    -- * Reading a file in the same shape
    getting,
    checkUnknown,
    checkRepeated,
    splitConf,
) where

import DNS.Config
import DNS.Log (Level (..))
import Data.IORef
import Data.List (nub, (\\))
import Network.Socket (PortNumber)
import System.IO.Error (ioeGetErrorString, ioeSetErrorString, tryIOError)

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data Config = Config
    { cnf_tcp_addrs :: [String]
    , cnf_tcp_port  :: PortNumber
    , cnf_udp_addrs :: [String]
    , cnf_udp_port  :: PortNumber
    , cnf_log       :: Bool
    , cnf_log_file  :: Maybe FilePath
    , cnf_log_level :: Level
    , cnf_clove_dir :: FilePath
    , cnf_tsig_file  :: FilePath
    , cnf_tcp_client_timeout :: Int
    , cnf_tcp_clients :: Int
    , cnf_transfers_out :: Int
    , cnf_transfer_time_limit :: Int
    } deriving (Show)

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_tcp_addrs = []
        , cnf_tcp_port  = 53
        , cnf_udp_addrs = []
        , cnf_udp_port  = 53
        , cnf_log       = True
        , cnf_log_file  = Nothing
        , cnf_log_level = WARNING
        , cnf_clove_dir = "/var/clove/"
        , cnf_tsig_file  = "tsig.conf"
        , cnf_tcp_client_timeout = 30
        , cnf_tcp_clients = 150
        , cnf_transfers_out = 10
        , cnf_transfer_time_limit = 3600 -- 1 hour
        }

----------------------------------------------------------------

data ZoneConf = ZoneConf
    { cnf_zone                  :: String
    , cnf_notify                :: Bool
    , cnf_notify_addrs          :: [String]
    , cnf_notify_port           :: PortNumber
    , cnf_notify_key            :: String
    , cnf_allow_notify          :: Bool
    , cnf_allow_notify_addrs    :: [String]
    , cnf_allow_notify_key      :: String
    , cnf_allow_transfer        :: Bool
    , cnf_allow_transfer_addrs  :: [String]
    , cnf_allow_transfer_key     :: String
    , cnf_source                :: String
    , cnf_source_port           :: PortNumber
    , cnf_source_key            :: String
    , cnf_signing               :: Bool
    , cnf_nsec3                 :: Bool
    , cnf_ksk_algo              :: String
    , cnf_ksk_size              :: Int
    , cnf_zsk_algo              :: String
    , cnf_zsk_size              :: Int
    , cnf_ds_digest             :: String
    , cnf_nsec3_hash            :: String
    , cnf_nsec3_optout          :: Bool
    -- ^ Whether to leave the delegations which carry no DS out of the
    --   NSEC3 chain.  RFC 9276 Sec 3.2 asks most zones not to: it
    --   shortens the chain by as many records as there are such
    --   delegations, which is worth having in a zone that is mostly
    --   delegations and nothing at all in a zone that is not, and what
    --   it costs is that a name in one of the gaps can no longer be
    --   denied.
    , cnf_rrsig_lifetime        :: Int
    , cnf_zsk_rollover_duration :: Int
    , cnf_zsk_preserve          :: Int
    }
    deriving (Show)

defaultZoneConf :: ZoneConf
defaultZoneConf =
    ZoneConf
        { cnf_zone                  = "example.org"
        , cnf_notify                = False
        , cnf_notify_addrs          = []
        , cnf_notify_port           = 53
        , cnf_notify_key            = ""
        , cnf_allow_notify          = False
        , cnf_allow_notify_addrs    = []
        , cnf_allow_notify_key      = ""
        , cnf_allow_transfer        = False
        , cnf_allow_transfer_addrs  = []
        , cnf_allow_transfer_key     = ""
        , cnf_signing               = True
        , cnf_source                = "example.zone"
        , cnf_source_port           = 53
        , cnf_source_key            = ""
        , cnf_nsec3                 = True
        , cnf_ksk_algo              = "ED25519"
        , cnf_ksk_size              = 0
        , cnf_zsk_algo              = "ED25519"
        , cnf_zsk_size              = 0
        , cnf_ds_digest             = "SHA-256"
        , cnf_nsec3_hash            = "SHA-1"
        , cnf_nsec3_optout          = False
        , cnf_rrsig_lifetime        = 864000 -- 10 days
        , cnf_zsk_rollover_duration = 604800 -- 7 days
        , cnf_zsk_preserve          = 10
        }

----------------------------------------------------------------

makeConfig :: Config -> [Conf] -> IO (Config, [ZoneConf])
makeConfig def conf0 = do
    ref <- newIORef []
    -- The signature keeps 'get' polymorphic under the monomorphism
    -- restriction.
    let get :: FromConf a => String -> (Config -> a) -> IO a
        get k func = getting ref conf k func def
    cnf_tcp_addrs <- get "tcp-addrs" cnf_tcp_addrs
    cnf_tcp_port  <- get "tcp-port"  cnf_tcp_port
    cnf_udp_addrs <- get "udp-addrs" cnf_udp_addrs
    cnf_udp_port  <- get "udp-port"  cnf_udp_port
    cnf_log       <- get "log"       cnf_log
    cnf_log_file  <- get "log-file"  cnf_log_file
    cnf_log_level <- get "log-level" cnf_log_level
    cnf_clove_dir <- get "clove-dir" cnf_clove_dir
    cnf_tsig_file <- get "tsig-file" cnf_tsig_file
    cnf_tcp_client_timeout <- get "tcp-client-timeout" cnf_tcp_client_timeout
    cnf_tcp_clients <- get "tcp-clients" cnf_tcp_clients
    cnf_transfers_out <- get "transfers-out" cnf_transfers_out
    cnf_transfer_time_limit <- get "transfer-time-limit" cnf_transfer_time_limit
    mapM_
        (uncurry checkPositive)
        [ ("tcp-client-timeout", cnf_tcp_client_timeout)
        , ("tcp-clients", cnf_tcp_clients)
        , ("transfers-out", cnf_transfers_out)
        , ("transfer-time-limit", cnf_transfer_time_limit)
        ]
    checkUnknown "" ref conf
    checkRepeated "" conf
    zonelist      <- mapM (makeZoneConf defaultZoneConf) zones
    pure (Config{..}, zonelist)
  where
    (conf, zones) = splitConf "zone" conf0

-- | None of the limits has a value which means "no limit".  A limit of
--   nothing at all would refuse every connection or every transfer, and
--   a negative one is not a limit; asking for one is the way to say how
--   much to put up with, not whether to.
checkPositive :: String -> Int -> IO ()
checkPositive what n
    | n > 0 = pure ()
    | otherwise = ioError $ userError $ what ++ ": must be positive, not " ++ show n

makeZoneConf :: ZoneConf -> [Conf] -> IO ZoneConf
makeZoneConf def conf = do
    ref <- newIORef []
    let get :: FromConf a => String -> (ZoneConf -> a) -> IO a
        get k func = getting ref conf k func def
    cnf_zone                  <- get "zone"                  cnf_zone
    cnf_notify                <- get "notify"                cnf_notify
    cnf_notify_addrs          <- get "notify-addrs"          cnf_notify_addrs
    cnf_notify_port           <- get "notify-port"           cnf_notify_port
    cnf_notify_key            <- get "notify-key"            cnf_notify_key
    cnf_allow_notify          <- get "allow-notify"          cnf_allow_notify
    cnf_allow_notify_addrs    <- get "allow-notify-addrs"    cnf_allow_notify_addrs
    cnf_allow_notify_key      <- get "allow-notify-key"      cnf_allow_notify_key
    cnf_allow_transfer        <- get "allow-transfer"        cnf_allow_transfer
    cnf_allow_transfer_addrs  <- get "allow-transfer-addrs"  cnf_allow_transfer_addrs
    cnf_allow_transfer_key     <- get "allow-transfer-key"    cnf_allow_transfer_key
    cnf_source                <- get "source"                cnf_source
    cnf_source_port           <- get "source-port"           cnf_source_port
    cnf_source_key            <- get "source-key"            cnf_source_key
    cnf_signing               <- get "signing"               cnf_signing
    cnf_nsec3                 <- get "nsec3"                 cnf_nsec3
    cnf_zsk_algo              <- get "zsk-algo"              cnf_zsk_algo
    cnf_zsk_size              <- get "zsk-size"              cnf_zsk_size
    cnf_ksk_algo              <- get "ksk-algo"              cnf_ksk_algo
    cnf_ksk_size              <- get "ksk-size"              cnf_ksk_size
    cnf_ds_digest             <- get "ds-digest"             cnf_ds_digest
    cnf_nsec3_hash            <- get "nsec3-hash"            cnf_nsec3_hash
    cnf_nsec3_optout          <- get "nsec3-optout"          cnf_nsec3_optout
    cnf_rrsig_lifetime        <- get "rrsig-lifetime"        cnf_rrsig_lifetime
    cnf_zsk_rollover_duration <- get "zsk-rollover-duration" cnf_zsk_rollover_duration
    cnf_zsk_preserve          <- get "zsk-preserve"          cnf_zsk_preserve
    checkUnknown (cnf_zone ++ ": ") ref conf
    checkRepeated (cnf_zone ++ ": ") conf
    pure ZoneConf{..}

{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | Reading one setting, remembering its name as a known one.
getting :: FromConf a => IORef [String] -> [Conf] -> String -> (b -> a) -> b -> IO a
getting ref conf k func def = do
    modifyIORef' ref (k :)
    et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
    let left e = ioError $ ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
    either left pure et

-- | Rejecting a setting we do not know.  Without this a misspelt name
--   is simply not found, the default is used and nothing is reported,
--   which is how \"log-devel\" and \"siging\" sat in the sample
--   configuration file without anybody noticing.
checkUnknown :: String -> IORef [String] -> [Conf] -> IO ()
checkUnknown label ref conf = do
    known <- readIORef ref
    case nub (map fst conf) \\ known of
        [] -> pure ()
        ks -> ioError $ userError $ label ++ "unknown setting: " ++ unwords ks

-- | Rejecting a setting which is given more than once.  The first one
--   is what is read, so a line added below an earlier one to change
--   something does nothing at all, and says nothing about it -- which is
--   a poor way to find out that a zone is still transferring to anybody
--   who asks.
checkRepeated :: String -> [Conf] -> IO ()
checkRepeated label conf = case nub (ks \\ nub ks) of
    [] -> pure ()
    ks' -> ioError $ userError $ label ++ "setting given more than once: " ++ unwords ks'
  where
    ks = map fst conf

loadConfig :: FilePath -> IO (Config, [ZoneConf])
loadConfig file = loadFile file >>= makeConfig defaultConfig

-- | Splitting a configuration into what comes before the first section
--   and the sections themselves, a section beginning at each occurrence
--   of the given setting.  Which is why every global setting has to be
--   written above the first zone.
splitConf :: String -> [Conf] -> ([Conf], [[Conf]])
splitConf key xs0 = (gs, zss)
  where
    p (k, _) = k == key
    (gs, os) = break p xs0
    zss = loop os
    loop [] = []
    loop (x : xs) =
        let (zs', xs') = break p xs
         in (x : zs') : loop xs'

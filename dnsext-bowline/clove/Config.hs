{-# LANGUAGE RecordWildCards #-}

module Config (
    Config (..),
    loadConfig,
    ZoneConf (..),
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
        }

----------------------------------------------------------------

data ZoneConf = ZoneConf
    { cnf_zone                 :: String
    , cnf_notify               :: Bool
    , cnf_notify_addrs         :: [String]
    , cnf_allow_notify         :: Bool
    , cnf_allow_notify_addrs   :: [String]
    , cnf_allow_transfer       :: Bool
    , cnf_allow_transfer_addrs :: [String]
    , cnf_source               :: String
    , cnf_signing              :: Bool
    , cnf_nsec3                :: Bool
    , cnf_ksk_algo             :: String
    , cnf_ksk_size             :: Int
    , cnf_zsk_algo             :: String
    , cnf_zsk_size             :: Int
    , cnf_ds_digest            :: String
    , cnf_nsec3_hash           :: String
    , cnf_rrsig_lifetime       :: Int
    }
    deriving (Show)

defaultZoneConf :: ZoneConf
defaultZoneConf =
    ZoneConf
        { cnf_zone                 = "example.org"
        , cnf_notify               = False
        , cnf_notify_addrs         = []
        , cnf_allow_notify         = False
        , cnf_allow_notify_addrs   = []
        , cnf_allow_transfer       = False
        , cnf_allow_transfer_addrs = []
        , cnf_signing              = True
        , cnf_source               = "example.zone"
        , cnf_nsec3                = True
        , cnf_ksk_algo             = "ED25519"
        , cnf_ksk_size             = 0
        , cnf_zsk_algo             = "ED25519"
        , cnf_zsk_size             = 0
        , cnf_ds_digest            = "SHA-256"
        , cnf_nsec3_hash           = "SHA-1"
        , cnf_rrsig_lifetime       = 604800 -- one week
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
    checkUnknown "" ref conf
    zonelist      <- mapM (makeZoneConf defaultZoneConf) zones
    pure (Config{..}, zonelist)
  where
    (conf, zones) = splitConfig conf0

makeZoneConf :: ZoneConf -> [Conf] -> IO ZoneConf
makeZoneConf def conf = do
    ref <- newIORef []
    let get :: FromConf a => String -> (ZoneConf -> a) -> IO a
        get k func = getting ref conf k func def
    cnf_zone                 <- get "zone"                 cnf_zone
    cnf_notify               <- get "notify"               cnf_notify
    cnf_notify_addrs         <- get "notify-addrs"         cnf_notify_addrs
    cnf_allow_notify         <- get "allow-notify"         cnf_allow_notify
    cnf_allow_notify_addrs   <- get "allow-notify-addrs"   cnf_allow_notify_addrs
    cnf_allow_transfer       <- get "allow-transfer"       cnf_allow_transfer
    cnf_allow_transfer_addrs <- get "allow-transfer-addrs" cnf_allow_transfer_addrs
    cnf_source               <- get "source"               cnf_source
    cnf_signing              <- get "signing"              cnf_signing
    cnf_nsec3                <- get "nsec3"                cnf_nsec3
    cnf_zsk_algo             <- get "zsk-algo"             cnf_zsk_algo
    cnf_zsk_size             <- get "zsk-size"             cnf_zsk_size
    cnf_ksk_algo             <- get "ksk-algo"             cnf_ksk_algo
    cnf_ksk_size             <- get "ksk-size"             cnf_ksk_size
    cnf_ds_digest            <- get "ds-digest"            cnf_ds_digest
    cnf_nsec3_hash           <- get "nsec3-hash"           cnf_nsec3_hash
    cnf_rrsig_lifetime       <- get "rrsig-lifetime"       cnf_rrsig_lifetime
    checkUnknown (cnf_zone ++ ": ") ref conf
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

loadConfig :: FilePath -> IO (Config, [ZoneConf])
loadConfig file = loadFile file >>= makeConfig defaultConfig

splitConfig :: [Conf] -> ([Conf], [[Conf]])
splitConfig xs0 = (gs, zss)
  where
    p (k, _) = k == "zone"
    (gs, os) = break p xs0
    zss = loop os
    loop [] = []
    loop (x : xs) =
        let (zs', xs') = break p xs
         in (x : zs') : loop xs'

{-# LANGUAGE RecordWildCards #-}

module Config (
    Config (..),
    loadConfig,
    ZoneConf (..),
) where

import DNS.Config
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
    , cnf_log_level :: String
    , cnf_clove_dir :: FilePath
    }

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_tcp_addrs = []
        , cnf_tcp_port  = 53
        , cnf_udp_addrs = []
        , cnf_udp_port  = 53
        , cnf_log       = False
        , cnf_log_file  = Nothing
        , cnf_log_level = "WARNING"
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
    , cnf_pub_algo             :: String
    , cnf_ds_digest            :: String
    , cnf_nsec3_hash           :: String
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
        , cnf_pub_algo             = "ED25519"
        , cnf_ds_digest            = "SHA-256"
        , cnf_nsec3_hash           = "SHA-1"
        }

----------------------------------------------------------------

makeConfig :: Config -> [Conf] -> IO (Config, [ZoneConf])
makeConfig def conf0 = do
    cnf_tcp_addrs <- get "tcp-addrs" cnf_tcp_addrs
    cnf_tcp_port  <- get "tcp-port"  cnf_tcp_port
    cnf_udp_addrs <- get "udp-addrs" cnf_udp_addrs
    cnf_udp_port  <- get "udp-port"  cnf_udp_port
    cnf_log       <- get "log"       cnf_log
    cnf_log_file  <- get "log-file"  cnf_log_file
    cnf_log_level <- get "log-level" cnf_log_level
    cnf_clove_dir <- get "clove-dir" cnf_clove_dir
    zonelist      <- mapM (makeZoneConf defaultZoneConf) zones
    pure (Config{..}, zonelist)
  where
    (conf, zones) = splitConfig conf0
    get k func = do
        et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
        let left e = do
                let e' = ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
                ioError e'
        either left pure et

makeZoneConf :: ZoneConf -> [Conf] -> IO ZoneConf
makeZoneConf def conf = do
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
    cnf_pub_algo             <- get "pub-algo"             cnf_pub_algo
    cnf_ds_digest            <- get "ds-digest"            cnf_ds_digest
    cnf_nsec3_hash           <- get "nsec3-hash"           cnf_nsec3_hash
    pure ZoneConf{..}
  where
    get k func = do
        et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
        let left e = do
                let e' = ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
                ioError e'
        either left pure et

{- FOURMOLU_ENABLE -}

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

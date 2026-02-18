{-# LANGUAGE RecordWildCards #-}

module Config (
    Config (..),
    loadConfig,
) where

import DNS.Config
import Network.Socket (PortNumber)
import System.IO.Error (ioeGetErrorString, ioeSetErrorString, tryIOError)

{- FOURMOLU_DISABLE -}
data Config = Config
    { cnf_zone                 :: String
    , cnf_source               :: String
    , cnf_dnssec               :: Bool
    , cnf_notify               :: Bool
    , cnf_notify_addrs         :: [String]
    , cnf_allow_transfer       :: Bool
    , cnf_allow_transfer_addrs :: [String]
    , cnf_tcp_addrs            :: [String]
    , cnf_tcp_port             :: PortNumber
    , cnf_udp_addrs            :: [String]
    , cnf_udp_port             :: PortNumber
    }

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_zone                 = "example.org"
        , cnf_source               = "example.zone"
        , cnf_dnssec               = False
        , cnf_notify               = False
        , cnf_notify_addrs         = []
        , cnf_allow_transfer       = False
        , cnf_allow_transfer_addrs = []
        , cnf_tcp_addrs            = ["127.0.0.1", "::1"]
        , cnf_tcp_port             = 53
        , cnf_udp_addrs            = ["127.0.0.1", "::1"]
        , cnf_udp_port             = 53
        }

makeConfig :: Config -> [Conf] -> IO Config
makeConfig def conf = do
    cnf_zone                 <- get "zone"                 cnf_zone
    cnf_source               <- get "source"               cnf_source
    cnf_dnssec               <- get "dnssec"               cnf_dnssec
    cnf_notify               <- get "notify"               cnf_notify
    cnf_notify_addrs         <- get "notify-addrs"         cnf_notify_addrs
    cnf_allow_transfer       <- get "allow-transfer"       cnf_allow_transfer
    cnf_allow_transfer_addrs <- get "allow-transfer-addrs" cnf_allow_transfer_addrs
    cnf_tcp_addrs            <- get "tcp-addrs"            cnf_tcp_addrs
    cnf_tcp_port             <- get "tcp-port"             cnf_tcp_port
    cnf_udp_addrs            <- get "udp-addrs"            cnf_udp_addrs
    cnf_udp_port             <- get "udp-port"             cnf_udp_port
    pure Config{..}
  where
    get k func = do
        et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
        let left e = do
                let e' = ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
                ioError e'
        either left pure et
{- FOURMOLU_ENABLE -}

loadConfig :: FilePath -> IO Config
loadConfig file = loadFile file >>= makeConfig defaultConfig

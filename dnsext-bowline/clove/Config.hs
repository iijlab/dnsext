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
    { cnf_zone_name      :: String
    , cnf_source         :: FilePath
    , cnf_transfer       :: Bool
    , cnf_transfer_addrs :: [String]
    , cnf_tcp_addrs      :: [String]
    , cnf_tcp_port       :: PortNumber
    , cnf_udp_addrs      :: [String]
    , cnf_udp_port       :: PortNumber
    }

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_zone_name      = "example.org"
        , cnf_source         = "example.conf"
        , cnf_transfer       = False
        , cnf_transfer_addrs = ["127.0.0.1", "::1"]
        , cnf_tcp_addrs      = ["127.0.0.1", "::1"]
        , cnf_tcp_port       = 53
        , cnf_udp_addrs      = ["127.0.0.1", "::1"]
        , cnf_udp_port       = 53
        }

makeConfig :: Config -> [Conf] -> IO Config
makeConfig def conf = do
    cnf_zone_name      <- get "zone-name"      cnf_zone_name
    cnf_source         <- get "source"         cnf_source
    cnf_transfer       <- get "transfer"       cnf_transfer
    cnf_transfer_addrs <- get "transfer-addrs" cnf_transfer_addrs
    cnf_tcp_addrs      <- get "tcp-addrs"      cnf_tcp_addrs
    cnf_tcp_port       <- get "tcp-port"       cnf_tcp_port
    cnf_udp_addrs      <- get "udp-addrs"      cnf_udp_addrs
    cnf_udp_port       <- get "udp-port"       cnf_udp_port
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

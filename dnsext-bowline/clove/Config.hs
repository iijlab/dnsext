{-# LANGUAGE RecordWildCards #-}

module Config (
    Config (..),
    loadConfig,
) where

import DNS.Config
import Network.Socket (PortNumber)
import System.IO.Error (ioeGetErrorString, ioeSetErrorString, tryIOError)

data Config = Config
    { cnf_zone_name :: String
    , cnf_zone_file :: FilePath
    , cnf_tcp_addrs :: [String]
    , cnf_tcp_port :: PortNumber
    , cnf_udp_addrs :: [String]
    , cnf_udp_port :: PortNumber
    , cnf_allow_axfr :: [String]
    }

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_zone_name = "example.org"
        , cnf_zone_file = "example.conf"
        , cnf_tcp_addrs = ["127.0.0.1", "::1"]
        , cnf_tcp_port = 53
        , cnf_udp_addrs = ["127.0.0.1", "::1"]
        , cnf_udp_port = 53
        , cnf_allow_axfr = ["127.0.0.1", "::1"]
        }

makeConfig :: Config -> [Conf] -> IO Config
makeConfig def conf = do
    cnf_zone_name <- get "zone-name" cnf_zone_name
    cnf_zone_file <- get "zone-file" cnf_zone_file
    cnf_tcp_addrs <- get "tcp-addrs" cnf_tcp_addrs
    cnf_tcp_port <- get "tcp-port" cnf_tcp_port
    cnf_udp_addrs <- get "udp-addrs" cnf_udp_addrs
    cnf_udp_port <- get "udp-port" cnf_udp_port
    cnf_allow_axfr <- get "allow-axfer" cnf_allow_axfr
    pure Config{..}
  where
    get k func = do
        et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
        let left e = do
                let e' = ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
                ioError e'
        either left pure et

loadConfig :: FilePath -> IO Config
loadConfig file = loadFile file >>= makeConfig defaultConfig

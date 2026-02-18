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
    , cnf_dns_addrs :: [String]
    , cnf_udp :: Bool
    , cnf_udp_port :: PortNumber
    }

defaultConfig :: Config
defaultConfig =
    Config
        { cnf_zone_name = "example.org"
        , cnf_zone_file = "example.conf"
        , cnf_dns_addrs = ["127.0.0.1", "::1"]
        , cnf_udp = True
        , cnf_udp_port = 53
        }

makeConfig :: Config -> [Conf] -> IO Config
makeConfig def conf = do
    cnf_zone_name <- get "zone-name" cnf_zone_name
    cnf_zone_file <- get "zone-file" cnf_zone_file
    cnf_dns_addrs <- get "dns-addrs" cnf_dns_addrs
    cnf_udp <- get "udp" cnf_udp
    cnf_udp_port <- get "udp-port" cnf_udp_port
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

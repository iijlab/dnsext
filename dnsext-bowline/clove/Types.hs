module Types where

import Data.ByteString (ByteString)
import Data.IORef
import Data.IP
import Data.IP.RouteTable
import Network.Socket

import DNS.Auth.Algorithm
import DNS.Log
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types

----------------------------------------------------------------

data Source
    = FromFile FilePath
    | FromUpstream4 IPv4
    | FromUpstream6 IPv6
    deriving (Eq, Show)

data Signing = Signing
    { signingKSKConfig :: KeyConfig
    , signingZSKConfig :: KeyConfig
    , signingN3P :: Maybe RD_NSEC3PARAM -- Nothing for NSEC
    }
    deriving (Eq, Show)

----------------------------------------------------------------

type WakeUp = IO ()
type TimeoutWait = Maybe Int -> IO () -- Nothing waits without timeout

data Zone = Zone
    { zoneName :: Domain
    , zoneSource :: Source
    , zoneSigning :: Maybe Signing
    , zoneDB :: DB
    , zoneReady :: Bool
    , zoneFromFile :: Bool
    , zoneNotifyAddrs :: [IP]
    , zoneAllowNotifyAddrs :: [IP]
    , zoneAllowTransfer4 :: IPRTable IPv4 Bool
    , zoneAllowTransfer6 :: IPRTable IPv6 Bool
    , zoneTimeoutWait :: TimeoutWait
    , zoneWakeUp :: WakeUp
    }

type ZoneAlist = [(Domain, IORef Zone)]

----------------------------------------------------------------

data Env = Env
    { envPutLines :: PutLines IO
    }

----------------------------------------------------------------

data Proto = Proto
    { recvQuery :: IO (ByteString, SockAddr)
    , sendReply :: SockAddr -> ByteString -> IO ()
    , allowAXFR :: SockAddr -> Domain -> ZoneAlist -> IO (Maybe Zone)
    , protoName :: String
    }

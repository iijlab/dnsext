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

data Signing = Signing KeyConfig (Maybe RD_NSEC3PARAM) -- Nothing for NSEC
    deriving (Eq, Show)

----------------------------------------------------------------

type WakeUp = IO ()
type Wait = Int -> IO ()

data Zone = Zone
    { zoneName :: Domain
    , zoneSource :: Source
    , zoneSigning :: Maybe Signing
    , zoneDB :: DB
    , zoneReady :: Bool
    , zoneShouldRefresh :: Bool
    , zoneNotifyAddrs :: [IP]
    , zoneAllowNotifyAddrs :: [IP]
    , zoneAllowTransfer4 :: IPRTable IPv4 Bool
    , zoneAllowTransfer6 :: IPRTable IPv6 Bool
    , zoneWait :: Int -> IO ()
    , zoneWakeUp :: IO ()
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

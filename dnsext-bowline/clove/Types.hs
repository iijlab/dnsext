module Types where

import Data.IP
import Data.IP.RouteTable
import Data.Word

import DNS.Auth.Algorithm

data Source
    = FromFile FilePath
    | FromUpstream4 IPv4
    | FromUpstream6 IPv6
    deriving (Eq, Show)

data Control = Control
    { ctlDB :: DB
    , ctlReady :: Bool
    , ctlShouldRefresh :: Bool
    , ctlNotifyAddrs :: [IP]
    , ctlAllowTransfer4 :: IPRTable IPv4 Bool
    , ctlAllowTransfer6 :: IPRTable IPv6 Bool
    }

type Serial = Word32

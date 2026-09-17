{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.ByteString (ByteString)
import Data.IORef
import Data.IP
import Data.IP.RouteTable as IPRT
import Network.Socket

import DNS.Auth.Algorithm
import DNS.Log
import DNS.SEC
import DNS.SEC.Verify
import DNS.TSIG (TSIGError, TSIGKey)
import DNS.Types

----------------------------------------------------------------

data Source
    = FromFile FilePath
    | FromUpstream4 IPv4 PortNumber
    | FromUpstream6 IPv6 PortNumber
    deriving (Eq, Show)

data Signing = Signing
    { signingKSKConfig :: KeyConfig
    , signingZSKConfig :: KeyConfig
    , signingZSKRollover :: Int
    -- ^ How long, in seconds, a ZSK is used before the next one is
    --   generated.  Not the same thing as the life time of an RRSIG.
    , signingZSKPreserve :: Int
    -- ^ How many ZSKs are kept on disk.  Generating one beyond this
    --   removes the oldest.
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
    , zoneRRs :: [ResourceRecord]
    -- ^ Records last obtained from the source, kept so that the zone
    --   can be signed again without transferring it again.
    , zoneReady :: Bool
    , zoneFromFile :: Bool
    , zoneNotifyAddrs :: [IP]
    , zoneNotifyPort :: PortNumber
    , zoneAllowNotifyAddrs :: [IP]
    , zoneNotifyKey :: Maybe TSIGKey
    -- ^ Key the notifies we send are signed with
    , zoneAllowNotifyKey :: Maybe TSIGKey
    -- ^ Key a notify must be signed with.  When there is one, the
    --   addresses are not consulted.
    , zoneSourceKey :: Maybe TSIGKey
    -- ^ Key the queries to the upstream are signed with
    , zoneTransferKey :: Maybe TSIGKey
    -- ^ Key a transfer must be signed with.  When there is one, the
    --   addresses below are not consulted: holding the key is what
    --   grants the transfer.
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

-- | What came of asking whether a transfer may go ahead.
data Transfer
    = -- | It may.  The MAC of the request, when it carried one, which
      --   the answer has to be bound to.
      TransferOk Zone (Maybe Opaque)
    | -- | It may not, and there is nothing more to say about it
      TransferRefused
    | -- | It carried a TSIG and the TSIG was not good
      TransferNotAuth TSIGError

data Proto = Proto
    { recvQuery :: IO (ByteString, SockAddr)
    , sendReply :: SockAddr -> ByteString -> IO ()
    , allowAXFR :: SockAddr -> ByteString -> DNSMessage -> ZoneAlist -> IO Transfer
    , protoName :: String
    , recvErrorFatal :: Bool
    -- ^ Whether a failing 'recvQuery' means that nothing more can ever
    --   be received.  True for a connection, False for a datagram
    --   socket, which stays usable after an error.
    , replyLimit :: DNSMessage -> Maybe Int
    -- ^ Largest reply which may be sent in answer to this query, if the
    --   transport limits it at all.
    }

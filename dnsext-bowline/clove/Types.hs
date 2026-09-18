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
import DNS.TSIG (TSIGKey)
import DNS.Types
import DNS.Types.Time (EpochTime)

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
    , zoneAnswered :: EpochTime
    -- ^ When the source last answered.  RFC 1035 Sec 3.3.13 counts the
    --   expire of the zone from it: a secondary whose source has said
    --   nothing for that long is no longer authoritative for the zone.
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

-- | What the TSIG on a message came to (RFC 8945 Sec 5.2).
--
--   Checking the TSIG says who sent a message; it does not say what
--   they may have.  That second question is answered further on, by
--   allow-transfer-key and allow-notify-key, on a message we have
--   already been able to place.  Either way the answer to a signed
--   message is signed with the same key, which Sec 5.3 requires.
data Sender
    = -- | The message carried no TSIG, so the answer carries none
      Unsigned
    | -- | The key it was signed with, its MAC, which the answer has to
      --   be bound to, and the time it was checked at
      SignedWith TSIGKey Opaque EpochTime

-- | The key a message was signed with, where it was signed at all.
senderKey :: Sender -> Maybe TSIGKey
senderKey Unsigned = Nothing
senderKey (SignedWith key _ _) = Just key

-- | What came of going to the upstream for the zone.
data FromUpstream
    = -- | It answered, and this is the zone
      Transferred [ResourceRecord]
    | -- | It answered, and has nothing newer than what we hold
      Unchanged
    | -- | It did not answer, or not in a way we would take.  Not an
      --   error: the zone goes on being served until it expires.
      Unreachable

-- | What came of asking whether a transfer may go ahead.
data Transfer
    = -- | It may
      TransferOk Zone
    | -- | It may not, and there is nothing more to say about it
      TransferRefused

data Proto = Proto
    { recvQuery :: IO (ByteString, SockAddr)
    , sendReply :: SockAddr -> ByteString -> IO ()
    , allowAXFR :: SockAddr -> Sender -> DNSMessage -> ZoneAlist -> IO Transfer
    , protoName :: String
    , recvErrorFatal :: Bool
    -- ^ Whether a failing 'recvQuery' means that nothing more can ever
    --   be received.  True for a connection, False for a datagram
    --   socket, which stays usable after an error.
    , replyLimit :: DNSMessage -> Maybe Int
    -- ^ Largest reply which may be sent in answer to this query, if the
    --   transport limits it at all.
    }

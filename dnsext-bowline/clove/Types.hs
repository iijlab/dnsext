{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.IORef
import Data.IP
import Data.IP.RouteTable as IPRT
import Network.Socket

import DNS.Auth.Algorithm
import DNS.Auth.DB (NSEC3Config (..), ZoneCheck (..))
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
    , signingN3P :: Maybe NSEC3Config -- Nothing for NSEC
    , signingSigner :: Maybe Domain
    -- ^ The zone to name in the signer field of the RRSIGs over this
    --   zone's data, where it is to be a zone which did not sign them.
    --   Only reachable with @--insecure@.
    }
    deriving (Eq, Show)

-- | Records a zone puts into its responses which it has no business
--   putting there: the additional section of a referral is where a
--   resolver is handed addresses nobody is authoritative for, and the
--   authority section is where it is handed a delegation, and the
--   answer section is where an address rides along with the CNAME that
--   points at it.  None of it is signed -- glue never is -- so a
--   resolver cannot tell these from the real thing by looking.  What it
--   does with them is the whole question, and this is how a scenario
--   asks it.
--
--   Only reachable with @--insecure@.
data Spoof = Spoof
    { spoofAnswer :: [ResourceRecord]
    , spoofAuthority :: [ResourceRecord]
    , spoofAdditional :: [ResourceRecord]
    , spoofNxdomain :: [Domain]
    -- ^ Names to answer NXDOMAIN, whatever the right answer would have
    --   been.  Everything else about the reply is left alone, the proof
    --   of what is really there included, so a signed zone ends up
    --   saying two things at once.
    }
    deriving (Eq, Show)

-- | A zone which sends only what it should.
noSpoof :: Spoof
noSpoof =
    Spoof
        { spoofAnswer = []
        , spoofAuthority = []
        , spoofAdditional = []
        , spoofNxdomain = []
        }

----------------------------------------------------------------

type WakeUp = IO ()
type TimeoutWait = Maybe Int -> IO () -- Nothing waits without timeout

data Zone = Zone
    { zoneName :: Domain
    , zoneSource :: Source
    , zoneSigning :: Maybe Signing
    , zoneSignedChildren :: [Domain]
    -- ^ The signed zones clove also serves which are delegated from
    --   this one.  What the parent owes each of them is a DS.
    , zoneSpoof :: Spoof
    -- ^ What this zone attaches to its responses beyond what it has to
    --   say.  Empty unless clove was started with @--insecure@.
    , zoneCheck :: ZoneCheck
    -- ^ Whether what a zone may not contain is refused.  'Unchecked'
    --   only where clove was started with @--insecure@, which is for
    --   showing a resolver a zone which is wrong on purpose.
    , zoneDB :: DB
    , zoneBatches :: IORef (Maybe [[ResourceRecord]])
    -- ^ How the zone is cut into messages for a transfer, once
    --   somebody has asked for one.  Working it out costs a pass over
    --   the zone with a good deal of encoding in it, and the answer is
    --   the same for every peer, so it is kept.  A new one of these is
    --   made wherever 'zoneDB' is, and never anywhere else, so the two
    --   cannot come apart.
    , zoneRRs :: [ResourceRecord]
    -- ^ Records last obtained from the source, kept so that the zone
    --   can be signed again without transferring it again.
    , zoneReady :: Bool
    , zoneFromFile :: Bool
    , zoneAnswered :: EpochTime
    -- ^ When the source last answered.  RFC 1035 Sec 3.3.13 counts the
    --   expire of the zone from it: a secondary whose source has said
    --   nothing for that long is no longer authoritative for the zone.
    , zoneFailing :: Bool
    -- ^ Whether the last attempt to reach the source failed.  RFC 1035
    --   Sec 3.3.13 has the retry interval, not the refresh interval,
    --   come after one that did.
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

-- | What a log line is about, for a server which holds several zones:
--   it says little by reporting that some file or other could not be
--   read.
zoneLabel :: Domain -> String
zoneLabel zone = toRepresentation zone ++ ": "

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

-- | How many of something may be going on at once.  A connection and a
--   transfer each hold one of these for as long as they last, so that a
--   peer, or a hundred of them, cannot have as many as it likes.
newtype Slots = Slots (TVar Int)

newSlots :: Int -> IO Slots
newSlots n = Slots <$> newTVarIO n

-- | One of them, and the action which gives it back, or 'Nothing'
--   where they are all taken.
takeSlot :: Slots -> IO (Maybe (IO ()))
takeSlot (Slots var) = atomically $ do
    free <- readTVar var
    if free <= 0
        then return Nothing
        else do
            writeTVar var $ free - 1
            return $ Just $ atomically $ modifyTVar' var (+ 1)

-- | Closing off an answer: encoding it for the transport it goes over,
--   and signing it where the query it answers was signed.
type Seal = DNSMessage -> ByteString

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
    , duringTransfer :: IO () -> IO ()
    -- ^ Handing a zone over, which is not governed by the same clock as
    --   waiting for a query: see 'Network.Run.TCP.Timeout' and the
    --   note on the TCP server.  Identity where no zone is ever handed
    --   over, which is every transport but TCP.
    , transferSlot :: IO (Maybe (IO ()))
    -- ^ Room to hand a zone over, and the action which gives the room
    --   back.  'Nothing' when as many transfers are already going on as
    --   the configuration allows.
    }

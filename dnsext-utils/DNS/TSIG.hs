{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Signing a DNS message with a secret shared with one peer, and
--   checking one so signed (RFC 8945).
--
--   TSIG authenticates a message between two parties who have agreed a
--   secret beforehand.  It is not DNSSEC and does not replace it: DNSSEC
--   signs the contents of a zone, TSIG signs one message between two
--   named peers, and a zone transfer needs the second whether or not it
--   has the first.
module DNS.TSIG (
    -- * Algorithms
    TSIGAlgorithm (..),
    algorithmName,
    algorithmFromName,
    macLength,

    -- * Keys
    TSIGKey (..),

    -- * Signing
    tsigMAC,
    defaultFudge,
    signTSIG,
    signTSIGCont,
    tsigPlaceholder,
    tsigRoom,

    -- * Checking
    TSIGError (..),
    fromTSIGError,
    toTSIGError,
    TSIGFault (..),
    TSIGResult (..),
    verifyTSIG,
    verifyTSIGCont,
    lastTSIG,
    checkMAC,
    checkTime,

    -- * Saying what was wrong
    errorTSIG,
    tsigReported,
) where

import Control.Monad (guard)
import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.MAC.HMAC as HMAC
import Data.Bits (shiftR, xor, (.|.))
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word16, Word64)

import DNS.Types
import DNS.Types.Encode (encodeResourceRecord)
import qualified DNS.Types.Opaque as Opaque
import DNS.Types.TSIG
import DNS.Types.Time (EpochTime)

----------------------------------------------------------------

-- | An algorithm a TSIG MAC may be taken with.
--
--   RFC 8945 Sec 6 requires HMAC-SHA1 and HMAC-SHA256 of every
--   implementation and recommends SHA256; the SHA2 sizes either side of
--   it are optional and cost nothing to carry.  HMAC-MD5 is left out:
--   the same section says it MUST NOT be used.
data TSIGAlgorithm
    = HMAC_SHA1
    | HMAC_SHA224
    | HMAC_SHA256
    | HMAC_SHA384
    | HMAC_SHA512
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The name an algorithm goes by on the wire.
algorithmName :: TSIGAlgorithm -> Domain
algorithmName HMAC_SHA1 = "hmac-sha1."
algorithmName HMAC_SHA224 = "hmac-sha224."
algorithmName HMAC_SHA256 = "hmac-sha256."
algorithmName HMAC_SHA384 = "hmac-sha384."
algorithmName HMAC_SHA512 = "hmac-sha512."

-- | Recognising an algorithm by its name.  A 'Domain' compares without
--   regard to case, so any spelling of it will do.
algorithmFromName :: Domain -> Maybe TSIGAlgorithm
algorithmFromName d = lookup d [(algorithmName a, a) | a <- [minBound ..]]

-- | Octets a MAC of this algorithm has when it is not truncated.
macLength :: TSIGAlgorithm -> Int
macLength HMAC_SHA1 = 20
macLength HMAC_SHA224 = 28
macLength HMAC_SHA256 = 32
macLength HMAC_SHA384 = 48
macLength HMAC_SHA512 = 64

----------------------------------------------------------------

-- | A key shared with one peer.
data TSIGKey = TSIGKey
    { tsigKeyName :: Domain
    -- ^ Name both ends know the key by
    , tsigKeyAlgorithm :: TSIGAlgorithm
    , tsigKeySecret :: BS.ByteString
    -- ^ The shared secret itself, as octets.  Written base64 wherever
    --   it is stored, by every implementation there is.
    }
    deriving (Eq, Show)

----------------------------------------------------------------

-- | Taking a MAC over the octets 'DNS.Types.TSIG.tsigDigest' assembled.
tsigMAC :: TSIGKey -> BS.ByteString -> Opaque
tsigMAC TSIGKey{..} digest = Opaque.fromByteString $ case tsigKeyAlgorithm of
    HMAC_SHA1 -> go Hash.SHA1
    HMAC_SHA224 -> go Hash.SHA224
    HMAC_SHA256 -> go Hash.SHA256
    HMAC_SHA384 -> go Hash.SHA384
    HMAC_SHA512 -> go Hash.SHA512
  where
    go :: Hash.HashAlgorithm a => a -> BS.ByteString
    go alg = convert $ HMAC.hmacGetDigest $ hmacWith alg tsigKeySecret digest

hmacWith :: Hash.HashAlgorithm a => a -> BS.ByteString -> BS.ByteString -> HMAC.HMAC a
hmacWith _ = HMAC.hmac

----------------------------------------------------------------

-- | Why a TSIG was not accepted (RFC 8945 Sec 5.2, Sec 2).  Each of
--   these travels in the Error field of the TSIG sent back, with an
--   RCODE of 'NotAuth'.
data TSIGError
    = -- | The MAC does not match
      BADSIG
    | -- | The key is not one we hold
      BADKEY
    | -- | The time signed is too far from ours
      BADTIME
    | -- | The MAC is shorter than the policy allows
      BADTRUNC
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | The number an error travels as.
fromTSIGError :: TSIGError -> Word16
fromTSIGError BADSIG = 16
fromTSIGError BADKEY = 17
fromTSIGError BADTIME = 18
fromTSIGError BADTRUNC = 22

-- | The error a number stands for, for reading one which arrived.
toTSIGError :: Word16 -> Maybe TSIGError
toTSIGError n = lookup n [(fromTSIGError e, e) | e <- [minBound ..]]

-- | Checking a MAC which arrived against the one the key gives.
--
--   A MAC may be sent truncated.  RFC 8945 Sec 5.2.2.1 will not have it
--   shorter than ten octets or half the algorithm's own length,
--   whichever is larger, and a shorter one is 'BADTRUNC' rather than
--   'BADSIG': the difference tells the far end to stop truncating
--   instead of to look at its secret.
--
--   The comparison is over the whole of the MAC which arrived, so a
--   truncated one is checked against as much of ours.
checkMAC
    :: TSIGKey
    -> BS.ByteString
    -- ^ the octets the MAC is taken over
    -> Opaque
    -- ^ the MAC which arrived
    -> Maybe TSIGError
checkMAC key digest arrived
    | got < shortest = Just BADTRUNC
    | BS.take got (Opaque.toByteString ours) `constantTimeEq` bs = Nothing
    | otherwise = Just BADSIG
  where
    bs = Opaque.toByteString arrived
    got = BS.length bs
    ours = tsigMAC key digest
    full = macLength $ tsigKeyAlgorithm key
    shortest = max 10 (full `div` 2)

-- | Comparing without letting the time taken say how much matched.
constantTimeEq :: BS.ByteString -> BS.ByteString -> Bool
constantTimeEq a b =
    BS.length a == BS.length b
        && 0 == foldl' (\acc (x, y) -> acc .|. (x `xor` y)) 0 (BS.zip a b)

-- | Checking the time a message was signed at (RFC 8945 Sec 5.2.3).
--   The two clocks must agree to within the fudge the sender asked for.
checkTime
    :: EpochTime
    -- ^ now
    -> Word64
    -- ^ time signed, as the record carries it
    -> Word16
    -- ^ fudge
    -> Maybe TSIGError
checkTime now signed fudge
    | abs (fromIntegral now - toInteger signed) <= toInteger fudge = Nothing
    | otherwise = Just BADTIME

----------------------------------------------------------------

-- | Seconds of difference between the clocks to put up with.  Three
--   hundred is what everybody uses and what everybody expects.
defaultFudge :: Word16
defaultFudge = 300

-- | The TSIG record to put at the end of a message, and the MAC in it,
--   which whatever follows will need.
--
--   The message must be given encoded, exactly as it will be sent,
--   before the record is added to it.  Adding the record to the message
--   and encoding that is what to do with the result -- the record goes
--   last, which RFC 8945 Sec 5.1 requires.
signTSIG
    :: TSIGKey
    -> EpochTime
    -- ^ now
    -> Word16
    -- ^ fudge
    -> Maybe Opaque
    -- ^ MAC of the request, when signing a response
    -> BS.ByteString
    -- ^ the message, encoded, without the record
    -> (ResourceRecord, Opaque)
signTSIG key now fudge mrequest body = (record (tsigKeyName key) rd, mac)
  where
    rd0 = emptyTSIG key now fudge body
    mac = tsigMAC key $ tsigDigest mrequest body (tsigKeyName key) rd0
    rd = rd0{tsig_mac = mac}

-- | The same for a message after the first one of a multi-message
--   response (RFC 8945 Sec 5.3.1).
signTSIGCont
    :: TSIGKey
    -> EpochTime
    -> Word16
    -> Opaque
    -- ^ MAC of the last message which carried a record
    -> [BS.ByteString]
    -- ^ the messages since then, this one last
    -> (ResourceRecord, Opaque)
signTSIGCont key now fudge prior bodies = (record (tsigKeyName key) rd, mac)
  where
    rd0 = emptyTSIG key now fudge $ lastOr "" bodies
    mac = tsigMAC key $ tsigDigestCont prior bodies rd0
    rd = rd0{tsig_mac = mac}

-- | A record of the shape RFC 8945 Sec 4.2 requires: the key for a
--   name, ANY for a class, nothing for a TTL.
record :: Domain -> RD_TSIG -> ResourceRecord
record name rd =
    ResourceRecord
        { rrname = name
        , rrtype = TSIG
        , rrclass = CL_ANY
        , rrttl = 0
        , rdata = toRData rd
        }

-- | Everything of a record but the MAC, which is not covered by itself.
emptyTSIG :: TSIGKey -> EpochTime -> Word16 -> BS.ByteString -> RD_TSIG
emptyTSIG key now fudge body =
    unsignedTSIG
        (algorithmName $ tsigKeyAlgorithm key)
        (fromIntegral now)
        fudge
        (identifierOf body)

-- | The identifier a message carries, read where it lies rather than
--   asked for again, so that it cannot be given a different one.
identifierOf :: BS.ByteString -> Word16
identifierOf bs
    | BS.length bs >= 2 = fromIntegral (BS.index bs 0) * 256 + fromIntegral (BS.index bs 1)
    | otherwise = 0

lastOr :: a -> [a] -> a
lastOr d [] = d
lastOr _ xs = last xs

----------------------------------------------------------------

-- | A record of the size and the shape a real one will have, with a
--   full length MAC of nothing in particular in it.  For measuring a
--   message before there is a signature to put on it: signing adds a
--   record, and a name, to whatever was measured without one.
tsigPlaceholder :: TSIGKey -> ResourceRecord
tsigPlaceholder key = record (tsigKeyName key) rd
  where
    alg = tsigKeyAlgorithm key
    rd =
        (unsignedTSIG (algorithmName alg) 0 defaultFudge 0)
            { tsig_mac = Opaque.fromByteString $ BS.replicate (macLength alg) 0
            }

-- | Octets a signature with this key takes up in a message, for leaving
--   room for one in a message which has to fit.  Measured with both
--   names written out in full, which is as much room as it can ever
--   take: in a message the owner name is usually a pointer into the
--   question instead.
tsigRoom :: TSIGKey -> Int
tsigRoom = BS.length . encodeResourceRecord . tsigPlaceholder

----------------------------------------------------------------

-- | What came of looking at the TSIG on a message.
data TSIGResult
    = -- | It is good.  The MAC, which whatever follows will need.
      TSIGOk Opaque
    | -- | There is no TSIG on it at all
      TSIGMissing
    | -- | There is one and it is not good
      TSIGFailed TSIGFault
    deriving (Eq, Show)

-- | What was wrong with a TSIG, and enough of the record it was wrong
--   on to say so in an answer (RFC 8945 Sec 5.2).  'errorTSIG' is what
--   turns one of these into the record which says it.
data TSIGFault = TSIGFault
    { faultError :: TSIGError
    , faultKeyName :: Domain
    -- ^ Name the message gave the key, which may be one we have not got
    , faultRecord :: RD_TSIG
    -- ^ The record as it arrived.  An answer gives its algorithm and its
    --   identifier back, and a complaint about the clocks gives its time
    --   and its fudge back as well.
    , faultKey :: Maybe TSIGKey
    -- ^ The key, where the trouble is not that we have not got it
    }
    deriving (Eq)

-- | The error and the name of the key, and nothing else: a 'TSIGFault'
--   holds a secret, and this is what goes in a log.
instance Show TSIGFault where
    show TSIGFault{..} = show faultError ++ " (key " ++ toRepresentation faultKeyName ++ ")"

-- | Checking the TSIG at the end of a message which arrived.
--
--   The message must be given exactly as it arrived, and the decoding
--   of that same message with it, so that neither is guessed at twice.
--   The checks are the ones RFC 8945 Sec 5.2 asks for, in its order:
--   the key first, then the MAC, then the time.
verifyTSIG
    :: (Domain -> Maybe TSIGKey)
    -- ^ the keys we hold, by name
    -> EpochTime
    -- ^ now
    -> Maybe Opaque
    -- ^ MAC of the request, when checking a response
    -> BS.ByteString
    -- ^ the message, exactly as it arrived
    -> DNSMessage
    -- ^ the same message, decoded
    -> TSIGResult
verifyTSIG keys now mrequest whole msg =
    withTSIG keys whole msg $ \key name rd body ->
        check key now name (tsigDigest mrequest body name rd) rd

-- | The same for a message after the first one of a multi-message
--   response.  The messages since the last record, this one last, are
--   what the MAC is taken over along with the timers.
verifyTSIGCont
    :: (Domain -> Maybe TSIGKey)
    -> EpochTime
    -> Opaque
    -- ^ MAC of the last message which carried a record
    -> [BS.ByteString]
    -- ^ the messages since then without their records, this one last
    -> BS.ByteString
    -- ^ this message, exactly as it arrived
    -> DNSMessage
    -> TSIGResult
verifyTSIGCont keys now prior earlier whole msg =
    withTSIG keys whole msg $ \key name rd body ->
        check key now name (tsigDigestCont prior (earlier ++ [body]) rd) rd

-- | Finding the record and the key it names, and handing them on.
withTSIG
    :: (Domain -> Maybe TSIGKey)
    -> BS.ByteString
    -> DNSMessage
    -> (TSIGKey -> Domain -> RD_TSIG -> BS.ByteString -> TSIGResult)
    -> TSIGResult
withTSIG keys whole msg k = case (lastTSIG msg, stripTSIG whole) of
    (Just (name, rd), Just body) -> case keys name of
        -- Sec 5.2.1: a key we do not know is BADKEY, and the name of a
        -- key is as much a part of it as the secret.
        Nothing -> failed BADKEY name rd Nothing
        Just key
            | tsig_algorithm rd /= algorithmName (tsigKeyAlgorithm key) ->
                failed BADKEY name rd Nothing
            | otherwise -> k key name rd body
    _ -> TSIGMissing

-- | The TSIG at the end of a message, if that is what is there.  The
--   name is the name of the key, which is what says which key to look
--   for -- 'verifyTSIG' finds it for itself, and this is for whoever
--   has to sign the answer with the same one.
lastTSIG :: DNSMessage -> Maybe (Domain, RD_TSIG)
lastTSIG msg = case reverse $ additional msg of
    rr : _ | rrtype rr == TSIG -> (,) (rrname rr) <$> fromRData (rdata rr)
    _ -> Nothing

check :: TSIGKey -> EpochTime -> Domain -> BS.ByteString -> RD_TSIG -> TSIGResult
check key now name digest rd = case checkMAC key digest (tsig_mac rd) of
    Just e -> bad e
    Nothing -> case checkTime now (tsig_time_signed rd) (tsig_fudge rd) of
        Just e -> bad e
        Nothing -> TSIGOk $ tsig_mac rd
  where
    bad e = failed e name rd $ Just key

failed :: TSIGError -> Domain -> RD_TSIG -> Maybe TSIGKey -> TSIGResult
failed e name rd mkey =
    TSIGFailed
        TSIGFault
            { faultError = e
            , faultKeyName = name
            , faultRecord = rd
            , faultKey = mkey
            }

----------------------------------------------------------------

-- | The TSIG for an answer which says the TSIG on the request was no
--   good (RFC 8945 Sec 5.2).  The answer itself carries RCODE 9
--   (NOTAUTH); this record is what names the check which failed, so that
--   the far end is told why and not merely that it was turned away.
--
--   'BADKEY' and 'BADSIG' mean we cannot tell who sent the request, so
--   the record carries no MAC at all: Sec 5.3.2 says such an answer MUST
--   NOT be signed, and Sec 5.4 has whoever receives it treat it as a
--   hint rather than as the truth.
--
--   'BADTIME' is not like that.  The key and the MAC were good and only
--   the clocks disagree, so the answer is signed with that key, it
--   carries our own time in the other data, and it gives back the time
--   and the fudge the request came with -- which is what lets the far
--   end check the answer without falling over the clocks a second time
--   (Sec 5.2.3).  'BADTRUNC' is signed as well, but not bound to a
--   request MAC we never accepted.
--
--   The message must be given encoded, exactly as it will be sent,
--   before the record is added to it, as for 'signTSIG'.
errorTSIG
    :: TSIGFault
    -> EpochTime
    -- ^ now
    -> BS.ByteString
    -- ^ the answer, encoded, without the record
    -> ResourceRecord
errorTSIG TSIGFault{..} now body = record faultKeyName rd
  where
    req = faultRecord
    -- Sec 5.2.3: a complaint about the clocks is signed at the time the
    -- request gave, so that what differs is the times and not the check.
    (time, fudge, other)
        | faultError == BADTIME =
            (tsig_time_signed req, tsig_fudge req, sixOctets now)
        | otherwise = (fromIntegral now, defaultFudge, Opaque.fromByteString "")
    rd0 =
        (unsignedTSIG (tsig_algorithm req) time fudge (identifierOf body))
            { tsig_error = fromTSIGError faultError
            , tsig_other = other
            }
    -- Sec 5.3.2: the request MAC goes into the digest only where it was
    -- found to be good, which is nowhere but the complaint about clocks.
    mrequest = if faultError == BADTIME then Just (tsig_mac req) else Nothing
    rd = case signWith of
        Nothing -> rd0
        Just key -> rd0{tsig_mac = tsigMAC key $ tsigDigest mrequest body faultKeyName rd0}
    signWith = case faultError of
        BADTIME -> faultKey
        BADTRUNC -> faultKey
        _ -> Nothing

-- | A time as the other data of a 'BADTIME' carries it: an unsigned 48
--   bit integer, most significant octet first (RFC 8945 Sec 5.2.3).
sixOctets :: EpochTime -> Opaque
sixOctets t = Opaque.fromByteString $ BS.pack [fromIntegral (n `shiftR` s) | s <- [40, 32, 24, 16, 8, 0]]
  where
    n = fromIntegral t :: Word64

-- | What the far end says was wrong with the TSIG we sent it (RFC 8945
--   Sec 5.4).  Such an answer usually carries an unsigned record, which
--   checking it as though it were an ordinary answer can only call
--   'BADTRUNC'; this reads what it says instead.
tsigReported :: DNSMessage -> Maybe TSIGError
tsigReported msg = do
    guard $ rcode msg == NotAuth
    (_, rd) <- lastTSIG msg
    toTSIGError $ tsig_error rd

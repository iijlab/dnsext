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

    -- * Checking
    TSIGError (..),
    fromTSIGError,
    checkMAC,
    checkTime,
) where

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.MAC.HMAC as HMAC
import Data.Bits (xor, (.|.))
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Word (Word16, Word64)

import DNS.Types
import qualified DNS.Types.Opaque as Opaque
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
    deriving (Eq, Ord, Show)

-- | The number an error travels as.
fromTSIGError :: TSIGError -> Word16
fromTSIGError BADSIG = 16
fromTSIGError BADKEY = 17
fromTSIGError BADTIME = 18
fromTSIGError BADTRUNC = 22

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

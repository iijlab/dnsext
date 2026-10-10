{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DNS.SEC.Verify.RSA (
    rsaSHA1,
    rsaSHA256,
    rsaSHA512,
    rsaEncodePriKey,
    rsaDecodePriKey,
    rsaDecodePubKey,
    rsaEncodePubKey,
    maxRSAKeyBits,
)
where

-- ram

-- crypton

import Crypto.Hash (HashAlgorithm)
import Crypto.Hash.Algorithms (SHA1 (..), SHA256 (..), SHA512 (..))
import Crypto.Number.Serialize (i2osp, os2ip)
import Crypto.PubKey.RSA (PrivateKey (..), PublicKey (..))
import qualified Crypto.PubKey.RSA as RSA
import Crypto.PubKey.RSA.PKCS15 (HashAlgorithmASN1, signSafer, verify)
import Crypto.Random.Types (MonadRandom)

import DNS.SEC.Types (PubKey (..))
import DNS.SEC.Verify.Types
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import Codec.Serialise
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)

instance Serialise RSA.PublicKey
instance Serialise RSA.PrivateKey

{- Verify RRSIG with DNSKEY using RSA/SHA-x
-- RSA/SHA-1 https://datatracker.ietf.org/doc/html/rfc3110
-- RSA/SHA-2 https://datatracker.ietf.org/doc/html/rfc5702
 -}

rsaSHA1, rsaSHA256, rsaSHA512 :: RRSIGImpl
rsaSHA1 = rsaSHAHelper SHA1
rsaSHA256 = rsaSHAHelper SHA256
rsaSHA512 = rsaSHAHelper SHA512

rsaSHAHelper :: (HashAlgorithm hash, HashAlgorithmASN1 hash) => hash -> RRSIGImpl
rsaSHAHelper alg =
    RRSIGImpl
        { rrsigIGenKeyPair = \sizeInBits -> RSA.generate (sizeInBits `div` 8) 65537
        , rrsigIEncodePriKey = rsaEncodePriKey
        , rrsigIDecodePriKey = rsaDecodePriKey
        , rrsigIEncodePubKey = rsaEncodePubKey
        , rrsigIDecodePubKey = rsaDecodePubKey
        , rrsigIEncodeSignature = rsaEncodeSignature
        , rrsigIDecodeSignature = rsaDecodeSignature
        , rrsigISign = unsafeRsaSign alg
        , rrsigIVerify = rsaVerify alg
        }

rsaEncodePriKey :: PrivateKey -> ByteString
rsaEncodePriKey = BL.toStrict . serialise

rsaDecodePriKey :: ByteString -> Either String PrivateKey
rsaDecodePriKey = Right . deserialise . BL.fromStrict

-- | Most bits of modulus, and of exponent, taken from a DNSKEY.
--   RFC 5702 Sec 2.1 and 2.2 hold an RSA key to 4096 bits, and RFC 3110
--   Sec 2 puts the same bound on the exponent.
maxRSAKeyBits :: Int
maxRSAKeyBits = 4096

rsaDecodePubKey :: PubKey -> Either String PublicKey
rsaDecodePubKey (PubKey o)
    | byteSize <= 0 =
        Left $ "RSASHA.rsaDecodePubKey: key size must be positive: " ++ show byteSize
    | r /= 0 =
        Left $
            "RSASHA.rsaDecodePubKey: size in bits is not multiple of 8 : bit-size = "
                ++ show bitSize
    | bitSize > maxRSAKeyBits = tooLarge "modulus" bitSize
    | expBits > maxRSAKeyBits = tooLarge "exponent" expBits
    | otherwise =
        Right
            PublicKey
                { public_size = byteSize
                , public_n = os2ip $ Opaque.toByteString n
                , public_e = os2ip $ Opaque.toByteString e
                }
  where
    tooLarge what bits =
        Left $
            "RSASHA.rsaDecodePubKey: "
                ++ what
                ++ " is larger than "
                ++ show maxRSAKeyBits
                ++ " bits: "
                ++ show bits
    expBits = Opaque.length e * 8
    (bitSize, e, n) = case Opaque.uncons o of
        Just (0, r0) -> fromJust $ do
            (x, r1) <- Opaque.uncons r0
            (y, r2) <- Opaque.uncons r1
            let elen = 256 * fromIntegral x + fromIntegral y
            return $ divide elen r2
        Just (l, r0) -> divide (fromIntegral l) r0
        _ -> error "toPubKey_RSA"

    divide elen o' =
        let (e', n') = Opaque.splitAt elen o'
         in ( Opaque.length n' * 8
            , e'
            , n'
            )
    (byteSize, r) = bitSize `quotRem` 8

rsaEncodePubKey :: PublicKey -> PubKey
rsaEncodePubKey PublicKey{..}
    | elen >= 256 =
        let (x, y) = elen `divMod` 256
         in PubKey $
                Opaque.concat
                    [ Opaque.singleton 0
                    , Opaque.singleton $ fromIntegral x
                    , Opaque.singleton $ fromIntegral y
                    , e
                    , n
                    ]
    | otherwise =
        PubKey $
            Opaque.concat
                [ Opaque.singleton $ fromIntegral elen
                , e
                , n
                ]
  where
    e = Opaque.fromByteString $ i2osp public_e
    n = Opaque.fromByteString $ i2osp public_n
    elen = Opaque.length e

type Signature = ByteString

rsaEncodeSignature :: Signature -> Opaque
rsaEncodeSignature = Opaque.fromByteString

rsaDecodeSignature :: Opaque -> Either String Signature
rsaDecodeSignature = Right . Opaque.toByteString

rsaVerify
    :: (HashAlgorithm hash, HashAlgorithmASN1 hash)
    => hash
    -> PublicKey
    -> Signature
    -> ByteString
    -> Either String Bool
rsaVerify alg pubkey sig msg =
    Right $ verify (Just alg) pubkey msg sig

unsafeRsaSign
    :: (HashAlgorithm hash, HashAlgorithmASN1 hash, MonadRandom m)
    => hash
    -> PrivateKey
    -> ByteString
    -> m Signature
unsafeRsaSign alg prikey msg = do
    ex <- signSafer (Just alg) prikey msg
    case ex of
        Left e -> error $ show e
        Right s -> return s

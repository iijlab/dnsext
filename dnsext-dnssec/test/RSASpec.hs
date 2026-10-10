{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Reading an RSA public key out of a DNSKEY.
module RSASpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Test.Hspec

import DNS.SEC
import DNS.SEC.Verify
import qualified DNS.Types.Opaque as Opaque

spec :: Spec
spec = describe "reading an RSA public key out of a DNSKEY" $ do
    it "takes a key of the size a zone really uses" $
        decoded (pubkey 17 2048) `shouldSatisfy` isRight

    it "takes a key at the largest size there is" $
        decoded (pubkey 4096 4096) `shouldSatisfy` isRight

    -- RFC 5702 Sec 2.1 and 2.2: an RSA key MUST NOT be more than 4096
    -- bits, and RFC 3110 Sec 2 bounds the exponent the same way.  The
    -- exponent is the field a verification costs time in.
    it "refuses a modulus larger than that" $
        decoded (pubkey 17 8192) `shouldSatisfy` isLeft

    it "refuses an exponent larger than that" $
        decoded (pubkey 8192 2048) `shouldSatisfy` isLeft

    -- A DNSKEY with fewer octets than the exponent's length field has
    -- no length to read, and the wire allows it: get_dnskey hands
    -- getPubKey (len - 4), so an RDLENGTH of 4 leaves no key at all.
    it "refuses a key with no exponent length in it" $
        decoded (toPubKey $ Opaque.fromByteString BS.empty) `shouldSatisfy` isLeft

    it "refuses a key whose two-octet length is cut short" $ do
        decoded (toPubKey $ Opaque.fromByteString $ BS.pack [0]) `shouldSatisfy` isLeft
        decoded (toPubKey $ Opaque.fromByteString $ BS.pack [0, 1]) `shouldSatisfy` isLeft

-- | What the RSA/SHA-256 implementation makes of this key.
decoded :: PubKey -> Either String ()
decoded k = case getRRSIGImpl RSASHA256 of
    Nothing -> Left "RSASHA256 is not supported here"
    Just RRSIGImpl{..} -> () <$ rrsigIDecodePubKey k

-- | A DNSKEY public key of the given exponent and modulus sizes, in
--   bits, laid out as RFC 3110 Sec 2 asks.
pubkey :: Int -> Int -> PubKey
pubkey eBits nBits = toPubKey $ Opaque.concat [elen, e, n]
  where
    e = ones $ (eBits + 7) `div` 8
    n = ones $ (nBits + 7) `div` 8
    len = Opaque.length e
    elen
        | len < 256 = Opaque.singleton $ fromIntegral len
        | otherwise =
            Opaque.concat
                [ Opaque.singleton 0
                , Opaque.singleton $ fromIntegral (len `div` 256)
                , Opaque.singleton $ fromIntegral (len `mod` 256)
                ]
    ones k = Opaque.fromByteString $ BS.replicate k 0xff

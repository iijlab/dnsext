{-# LANGUAGE RecordWildCards #-}

-- | Reading an RSA public key out of a DNSKEY.
module RSASpec (spec) where

import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Test.Hspec

import DNS.SEC
import DNS.SEC.Verify
import qualified DNS.Types.Opaque as Opaque

spec :: Spec
spec = describe "reading an RSA public key out of a DNSKEY" $ do
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

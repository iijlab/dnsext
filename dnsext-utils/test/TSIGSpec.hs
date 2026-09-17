{-# LANGUAGE OverloadedStrings #-}

module TSIGSpec (spec) where

import qualified Data.ByteString as BS
import Numeric (showHex)
import Test.Hspec

import DNS.TSIG
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

spec :: Spec
spec = do
    -- The HMACs themselves come from crypton; these say that each
    -- algorithm is wired to the one it is named after, against the
    -- vectors of RFC 2202 Sec 3 and RFC 4231 Sec 4.
    describe "the MAC of each algorithm" $ do
        it "HMAC-SHA1 matches RFC 2202 case 1" $
            mac HMAC_SHA1 (BS.replicate 20 0x0b) "Hi There"
                `shouldBe` "b617318655057264e28bc0b6fb378c8ef146be00"

        it "HMAC-SHA224 matches RFC 4231 case 1" $
            mac HMAC_SHA224 (BS.replicate 20 0x0b) "Hi There"
                `shouldBe` "896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22"

        it "HMAC-SHA256 matches RFC 4231 case 1" $
            mac HMAC_SHA256 (BS.replicate 20 0x0b) "Hi There"
                `shouldBe` "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"

        it "HMAC-SHA384 matches RFC 4231 case 2" $
            mac HMAC_SHA384 "Jefe" "what do ya want for nothing?"
                `shouldBe` "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e"
                    ++ "8e2240ca5e69e2c78b3239ecfab21649"

        it "HMAC-SHA512 matches RFC 4231 case 2" $
            mac HMAC_SHA512 "Jefe" "what do ya want for nothing?"
                `shouldBe` "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea250554"
                    ++ "9758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"

        it "gives a MAC of the length the algorithm says" $
            mapM_
                (\a -> BS.length (macBytes a "secret" "message") `shouldBe` macLength a)
                [minBound ..]

    describe "algorithm names" $ do
        it "round trips" $
            mapM_ (\a -> algorithmFromName (algorithmName a) `shouldBe` Just a) [minBound ..]

        it "does not mind the case they are written in" $
            algorithmFromName "HMAC-SHA256." `shouldBe` Just HMAC_SHA256

        it "does not know HMAC-MD5, which RFC 8945 Sec 6 forbids" $
            algorithmFromName "hmac-md5.sig-alg.reg.int." `shouldBe` Nothing

    describe "checking a MAC" $ do
        it "accepts the MAC the key gives" $
            checkMAC key digest (tsigMAC key digest) `shouldBe` Nothing

        it "rejects one taken with another secret" $
            checkMAC key digest (tsigMAC other digest) `shouldBe` Just BADSIG

        it "rejects one taken over other octets" $
            checkMAC key digest (tsigMAC key "something else") `shouldBe` Just BADSIG

        -- RFC 8945 Sec 5.2.2.1: not shorter than ten octets nor than
        -- half the algorithm's own length, whichever is larger.  For
        -- SHA256 that is sixteen.
        it "accepts one truncated to half the hash" $
            checkMAC key digest (truncated 16) `shouldBe` Nothing

        it "rejects one truncated below half the hash" $
            checkMAC key digest (truncated 15) `shouldBe` Just BADTRUNC

        it "rejects a truncation that is wrong as BADSIG, not BADTRUNC" $
            checkMAC key digest (Opaque.fromByteString $ BS.replicate 16 0xff)
                `shouldBe` Just BADSIG

        it "will not take ten octets from a long hash" $
            -- Half of SHA512 is 32, so ten is far too few for it.
            checkMAC key512 digest (Opaque.fromByteString $ BS.replicate 10 0) `shouldBe` Just BADTRUNC

    describe "checking the time" $ do
        it "accepts a time within the fudge" $ do
            checkTime 1000 1000 300 `shouldBe` Nothing
            checkTime 1000 1300 300 `shouldBe` Nothing
            checkTime 1000 700 300 `shouldBe` Nothing

        it "rejects one outside it, either way" $ do
            checkTime 1000 1301 300 `shouldBe` Just BADTIME
            checkTime 1000 699 300 `shouldBe` Just BADTIME

    describe "error numbers" $
        it "are the ones RFC 8945 assigns" $ do
            fromTSIGError BADSIG `shouldBe` 16
            fromTSIGError BADKEY `shouldBe` 17
            fromTSIGError BADTIME `shouldBe` 18
            fromTSIGError BADTRUNC `shouldBe` 22

----------------------------------------------------------------

key, other, key512 :: TSIGKey
key = TSIGKey "k.example.jp." HMAC_SHA256 "a secret"
other = key{tsigKeySecret = "another secret"}
key512 = key{tsigKeyAlgorithm = HMAC_SHA512}

digest :: BS.ByteString
digest = "the octets a MAC is taken over"

truncated :: Int -> Opaque
truncated n = Opaque.fromByteString $ BS.take n $ Opaque.toByteString $ tsigMAC key digest

macBytes :: TSIGAlgorithm -> BS.ByteString -> BS.ByteString -> BS.ByteString
macBytes alg secret msg = Opaque.toByteString $ tsigMAC (TSIGKey "k." alg secret) msg

mac :: TSIGAlgorithm -> BS.ByteString -> BS.ByteString -> String
mac alg secret msg = concatMap (pad . flip showHex "") $ BS.unpack $ macBytes alg secret msg
  where
    pad s = if length s == 1 then '0' : s else s

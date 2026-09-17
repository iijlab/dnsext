{-# LANGUAGE OverloadedStrings #-}

module TSIGSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word16, Word64)
import Numeric (showHex)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Gen, arbitrary, forAll, listOf)

import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import qualified DNS.Types.Opaque as Opaque
import DNS.Types.TSIG

spec :: Spec
spec = do
    describe "TSIG" $ do
        it "is type 250" $
            fromTYPE TSIG `shouldBe` 250

        it "is named in the type table" $
            show TSIG `shouldBe` "TSIG"

        it "survives a round trip through a message" $
            roundTrip sample `shouldBe` Right sample

        -- RFC 8945 Sec 4.2: the MAC and the Other Data fields are
        -- carried with a length of their own, so an empty one and a
        -- long one have to come back as they went in.
        it "survives a round trip with an empty MAC and no other data" $
            roundTrip bare `shouldBe` Right bare

        prop "survives a round trip whatever the MAC" $
            forAll genOpaque $ \mac ->
                let rd = mkTSIG 0 mac 0 (Opaque.fromByteString "")
                 in roundTrip rd == Right rd

        prop "survives a round trip whatever the time" $
            forAll (arbitrary :: Gen Word64) $ \w ->
                -- The time is 48 bits on the wire, so that is all that
                -- can be asked to come back.
                let rd = mkTSIG (w `mod` 0x1000000000000) (Opaque.fromByteString "") 0 (Opaque.fromByteString "")
                 in roundTrip rd == Right rd

    -- Each of these was worked out a field at a time from RFC 8945
    -- outside Haskell, so that the two constructions have nothing in
    -- common but the RFC.
    describe "the octets a MAC is taken over" $ do
        -- Sec 4.3.3.  The key name is given in mixed case to show that
        -- what goes in is the canonical, lower case form.
        it "lays out the variables" $
            hex (tsigVariables "KEY.Example.JP." rd)
                `shouldBe` "036b6579076578616d706c65026a70"
                    ++ "0000ff"
                    ++ "00000000"
                    ++ "0b686d61632d73686132353600"
                    ++ "0000f1e2d3c4"
                    ++ "012c"
                    ++ "0000"
                    ++ "0000"

        -- Sec 4.3.1
        it "puts a length in front of a MAC" $
            hex (tsigMacField mac) `shouldBe` "0020" ++ hex (opaqueBytes mac)

        -- Sec 5.3.1
        it "lays out the timers" $
            hex (tsigTimers rd) `shouldBe` "0000f1e2d3c4" ++ "012c"

        it "signs a request as the message then the variables" $
            tsigDigest Nothing body "key.example.jp." rd
                `shouldBe` body <> tsigVariables "key.example.jp." rd

        it "signs a response with the request's MAC in front" $
            tsigDigest (Just mac) body "key.example.jp." rd
                `shouldBe` tsigMacField mac <> body <> tsigVariables "key.example.jp." rd

        it "signs a later message of a response with the timers alone" $
            tsigDigestCont mac [body, body] rd
                `shouldBe` tsigMacField mac <> body <> body <> tsigTimers rd

----------------------------------------------------------------

rd :: RD_TSIG
rd = case fromRData sample of
    Just t -> t
    Nothing -> error "sample is not a TSIG"

mac :: Opaque
mac = Opaque.fromByteString $ BS.pack [1 .. 32]

body :: BS.ByteString
body = BS.pack $ concat $ replicate 7 [0xab, 0xcd, 0xef]

opaqueBytes :: Opaque -> BS.ByteString
opaqueBytes = Opaque.toByteString

hex :: BS.ByteString -> String
hex = concatMap (pad . flip showHex "") . BS.unpack
  where
    pad s = if length s == 1 then '0' : s else s

-- | A TSIG as it would be made for a message.
mkTSIG :: Word64 -> Opaque -> Word16 -> Opaque -> RData
mkTSIG t mac err other = rd_tsig "hmac-sha256." t 300 mac 0xbeef err other

sample :: RData
sample = mkTSIG 0x0000f1e2d3c4 (Opaque.fromByteString $ BS.pack [1 .. 32]) 0 (Opaque.fromByteString "")

-- | Nothing in either variable length field but a reason, which is what
--   a BADTIME answer carries.
bare :: RData
bare =
    mkTSIG
        0
        (Opaque.fromByteString "")
        18
        (Opaque.fromByteString $ BS.pack [0, 0, 0x65, 0x43, 0x21, 0x00])

genOpaque :: Gen Opaque
genOpaque = Opaque.fromByteString . BS.pack <$> listOf arbitrary

-- | Encoding the record into a message and taking it out again, which
--   is the only way an RData is ever seen on the wire.
roundTrip :: RData -> Either DNSError RData
roundTrip rd = case decode $ encode msg of
    Left e -> Left e
    Right msg' -> case additional msg' of
        [rr] -> Right $ rdata rr
        _ -> Left $ DecodeError "expected exactly one record"
  where
    msg =
        defaultQuery
            { question = Question "example.jp." A IN
            , additional = [tsigRR]
            }
    tsigRR =
        ResourceRecord
            { rrname = "key.example.jp."
            , rrtype = TSIG
            , rrclass = CL_ANY
            , rrttl = 0
            , rdata = rd
            }

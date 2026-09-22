{-# LANGUAGE OverloadedStrings #-}

-- | Pairing NSEC and NSEC3 records with the signatures over them.
module NSECxRangeSpec (spec) where

import Data.Either (isRight)
import Test.Hspec

import DNS.SEC
import DNS.SEC.Verify (NSEC3_Range, zipSigsNSEC3)
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

spec :: Spec
spec = describe "pairing an NSECx record with its signatures" $ do
    it "pairs one record with the signature over it" $
        zipped [nsec3 "a.example.", sig "a.example."] `shouldSatisfy` isRight

    it "pairs each of several records with its own" $
        fmap length (zipped [nsec3 "a.example.", nsec3 "b.example.", sig "a.example.", sig "b.example."])
            `shouldBe` Right 2

    -- RFC 2181 Sec 5 does not allow an RRset to hold the same record
    -- twice, so a server which sends it twice is wrong -- but it is
    -- wrong in a way which takes nothing away.  The signature over the
    -- RRset is the same signature, and RFC 4034 Sec 6.3 has the
    -- duplicate dropped when the RRset is put in canonical form to be
    -- verified against it.
    it "pairs a record which arrived twice with the one signature over it" $
        fmap length (zipped [nsec3 "a.example.", nsec3 "a.example.", sig "a.example."])
            `shouldBe` Right 1

    it "still reports a record with no signature at all" $
        zipped [nsec3 "a.example.", nsec3 "b.example.", sig "a.example."]
            `shouldSatisfy` not . isRight

    it "still reports a signature with no record under it" $
        zipped [nsec3 "a.example.", sig "a.example.", sig "b.example."]
            `shouldSatisfy` not . isRight

-- | What 'zipSigsNSEC3' made of these records, as an 'Either'.
zipped :: [ResourceRecord] -> Either String [(ResourceRecord, NSEC3_Range, [(RD_RRSIG, TTL)])]
zipped rrs = zipSigsNSEC3 rrs Left Right

nsec3 :: Domain -> ResourceRecord
nsec3 name =
    ResourceRecord name NSEC3 IN 3600 $
        rd_nsec3 Hash_SHA1 [] 0 (Opaque.fromByteString "") (Opaque.fromByteString "next") [A]

sig :: Domain -> ResourceRecord
sig name =
    ResourceRecord name RRSIG IN 3600 $
        rd_rrsig NSEC3 ED25519 2 3600 4102444800 1577836800 12345 "example." (Opaque.fromByteString "")

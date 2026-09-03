{-# LANGUAGE OverloadedStrings #-}

module RFC5155Spec where

import Test.Hspec

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import Data.Either

import Common

spec :: Spec
spec = describe "authoritative algorithm" $ do
    runIO $ runInitIO $ addResourceDataForDNSSEC
    let zone = "example."
    db <- runIO $ do
        rrs <- loadZoneFile zone "test/rfc5155.zone"
        (_pub, _pri, dnskey, _ds, doSign) <-
            prepareDNSSEC $
                KeyConfig
                    { keyConfZone = zone
                    , keyConfPubAlg = ED25519
                    , keyConfDigestAlg = SHA256
                    , keyConfTTL = 3600
                    , keyConfDuration = 86400
                    }
        let salt = fromRight (error "fromBase16") $ Opaque.fromBase16 "aabbccdd"
            n3p = RD_NSEC3PARAM Hash_SHA1 0 12 salt

        makeDBforPrimary zone (Just n3p) doSign (rrs ++ [dnskey])
    doit db
    db2 <- runIO (makeDBforSecondary zone $ dbAll db)
    doit db2

-- fixme
-- db2 <- fromJust <$> runIO (makeDBforSecondary zone $ dbAll db)
-- doit db2

doit :: DB -> Spec
doit db = do
    it "builds two entries for the last range" $ do
        lookupN "00000000000000000000000000000000.example." db `shouldSatisfy` include "t644ebqk9bibcna874givr6joj62mlhv.example." NSEC3
    it "passes the test in Appendix B.1 (Name Error)" $ do
        let query = dnssecQuery{question = Question "a.c.x.w.example." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 8
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom.example." NSEC3
        authority ans `shouldSatisfy` includeRRSIG "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom.example." NSEC3
        authority ans `shouldSatisfy` include "b4um86eghhds6nea196smvmlo4ors995.example." NSEC3
        authority ans `shouldSatisfy` includeRRSIG "b4um86eghhds6nea196smvmlo4ors995.example." NSEC3
        authority ans `shouldSatisfy` include "35mthgpgcu1qg68fab165klnsnk3dpvl.example." NSEC3
        authority ans `shouldSatisfy` includeRRSIG "35mthgpgcu1qg68fab165klnsnk3dpvl.example." NSEC3
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

    it "passes the test in Appendix B.2 (No Data)" $ do
        let query = dnssecQuery{question = Question "ns1.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "2t7b4g4vsa5smi47k61mv5bv1a22bojr.example." NSEC3
        authority ans `shouldSatisfy` includeRRSIG "2t7b4g4vsa5smi47k61mv5bv1a22bojr.example." NSEC3
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

    it "passes the test in Appendix B.3 (Referral to an Opt-Out Unsigned Zone)" $ do
        let query = dnssecQuery{question = Question "mc.c.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 6
        authority ans `shouldSatisfy` includeNS "c.example." "ns1.c.example."
        authority ans `shouldSatisfy` includeNS "c.example." "ns2.c.example."
        authority ans `shouldSatisfy` include "35mthgpgcu1qg68fab165klnsnk3dpvl.example." NSEC3
        authority ans `shouldSatisfy` includeRRSIG "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom.example." NSEC3
        authority ans `shouldSatisfy` include "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom.example." NSEC3
        authority ans `shouldSatisfy` includeRRSIG "35mthgpgcu1qg68fab165klnsnk3dpvl.example." NSEC3
        length (additional ans) `shouldBe` 2
        flags ans `shouldSatisfy` not . authAnswer

    it "passes the test in Appendix B.4 (Wildcard Expansion)" $ do
        let query = dnssecQuery{question = Question "a.z.w.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "a.z.w.example." MX
        answer ans `shouldSatisfy` includeRRSIG "a.z.w.example." MX
        length (authority ans) `shouldBe` 2
        authority ans `shouldSatisfy` include "q04jkcevqvmu85r014c7dkba38o0ji5r.example" NSEC3
        authority ans `shouldSatisfy` includeRRSIG "q04jkcevqvmu85r014c7dkba38o0ji5r.example" NSEC3
        length (additional ans) `shouldBe` 4
        flags ans `shouldSatisfy` authAnswer

    it "passes the test in Appendix B.5 (Wildcard No Data Error)" $ do
        let query = dnssecQuery{question = Question "a.z.w.example." AAAA IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 8
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "k8udemvp1j2f7eg6jebps17vp3n8i58h.example" NSEC3
        authority ans `shouldSatisfy` includeRRSIG "k8udemvp1j2f7eg6jebps17vp3n8i58h.example" NSEC3
        authority ans `shouldSatisfy` include "q04jkcevqvmu85r014c7dkba38o0ji5r.example" NSEC3
        authority ans `shouldSatisfy` includeRRSIG "q04jkcevqvmu85r014c7dkba38o0ji5r.example" NSEC3
        authority ans `shouldSatisfy` include "r53bq7cc2uvmubfu5ocmm6pers9tk9en.example" NSEC3
        authority ans `shouldSatisfy` includeRRSIG "r53bq7cc2uvmubfu5ocmm6pers9tk9en.example" NSEC3
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

    it "passes the test in Appendix B.6 (DS Child Zone No Data Error)" $ do
        let query = dnssecQuery{question = Question "example." DS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom.example" NSEC3
        authority ans `shouldSatisfy` includeRRSIG "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom.example" NSEC3
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

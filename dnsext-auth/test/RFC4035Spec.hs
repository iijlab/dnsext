{-# LANGUAGE OverloadedStrings #-}

module RFC4035Spec where

import Test.Hspec

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types

import Common

spec :: Spec
spec = describe "authoritative algorithm" $ do
    runIO $ runInitIO $ addResourceDataForDNSSEC
    let zone = "example."
    db <- runIO $ do
        rrs <- loadZoneFile zone "test/rfc4035.zone"
        (_pub, _pri, dnskey, _ds, doSign) <-
            prepareDNSSEC $
                KeyConfig
                    { keyConfZone = zone
                    , keyConfPubAlg = ED25519
                    , keyConfDigestAlg = SHA256
                    , keyConfTTL = 3600
                    , keyConfDuration = 86400
                    }
        makeDBforPrimary zone Nothing doSign (rrs ++ [dnskey])
    doit db
    db2 <- runIO (makeDBforSecondary zone $ dbAll db)
    doit db2

-- Canonical order:
-- example.
-- -- *.example.
-- a.example.
-- ns1.a.example.
-- ns2.a.example.
-- ai.example.
-- b.example.
-- -- ml.example.
-- ns1.example.
-- ns2.example.
-- xx.example.
-- "*.w.example."
-- x.w.example.
-- x.y.w.example.

doit :: DB -> Spec
doit db = do
    it "passes the test in Appendix B.1 (Answer)" $ do
        -- Sec 3.1.1.  Including RRSIG RRs in a Response
        let query = dnssecQuery{question = Question "x.w.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "x.w.example." MX
        answer ans `shouldSatisfy` includeRRSIG "x.w.example." MX
        length (authority ans) `shouldBe` 0
        -- Our algorithm does not return NS in this case
        --        authority ans `shouldSatisfy` include "ns1.example." NS
        --        authority ans `shouldSatisfy` include "ns2.example." NS
        --        authority ans `shouldSatisfy` includeRRSIG "ns1.example." NS
        length (additional ans) `shouldBe` 4
        additional ans `shouldSatisfy` include "xx.example." A
        additional ans `shouldSatisfy` includeRRSIG "xx.example." A
        additional ans `shouldSatisfy` include "xx.example." AAAA
        additional ans `shouldSatisfy` includeRRSIG "xx.example." AAAA
        -- See above.
        flags ans `shouldSatisfy` authAnswer
    it "passes the test in Appendix B.2 (Name Error)" $ do
        -- Sec 3.1.3.2.  Including NSEC RRs: Name Error Response
        let query = dnssecQuery{question = Question "ml.example." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 6
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        -- for ml.example.
        authority ans `shouldSatisfy` include "b.example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "b.example." NSEC
        -- for *.example.
        authority ans `shouldSatisfy` include "example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "example." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "passes the test in Appendix B.3 (No Data Error)" $ do
        -- Sec 3.1.3.1.  Including NSEC RRs: No Data Response
        let query = dnssecQuery{question = Question "ns1.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "ns1.example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "ns1.example." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "passes the test in Appendix B.4 (Referral to Signed Zone)" $ do
        -- Sec 3.1.4.  Including DS RRs in a Response
        let query = dnssecQuery{question = Question "mc.a.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` includeNS "a.example." "ns1.a.example."
        authority ans `shouldSatisfy` includeNS "a.example." "ns2.a.example."
        authority ans `shouldSatisfy` include "a.example." DS
        authority ans `shouldSatisfy` includeRRSIG "a.example." DS
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns1.a.example." A
        additional ans `shouldSatisfy` include "ns2.a.example." A
        flags ans `shouldSatisfy` not . authAnswer
    it "passes the test in Appendix B.5 (Referral to Unsigned Zone)" $ do
        -- Sec 3.1.4.  Including DS RRs in a Response (no DS)
        let query = dnssecQuery{question = Question "mc.b.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` includeNS "b.example." "ns1.b.example."
        authority ans `shouldSatisfy` includeNS "b.example." "ns2.b.example."
        authority ans `shouldSatisfy` include "b.example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "b.example." NSEC
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns1.b.example." A
        additional ans `shouldSatisfy` include "ns2.b.example." A
        flags ans `shouldSatisfy` not . authAnswer
    it "passes the test in Appendix B.6 (Wildcard Expansion)" $ do
        -- Sec 3.1.3.3.  Including NSEC RRs: Wildcard Answer Response
        let query = dnssecQuery{question = Question "a.z.w.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "a.z.w.example." MX
        answer ans `shouldSatisfy` includeRRSIG "a.z.w.example." MX
        length (authority ans) `shouldBe` 2
        -- Our algorithm does not return NS in this case
        --  authority ans `shouldSatisfy` includeNS "ns1.example."
        --  authority ans `shouldSatisfy` includeNS "ns2.example."
        --  authority ans `shouldSatisfy` includeRRSIG "example." NS
        authority ans `shouldSatisfy` include "x.y.w.example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "x.y.w.example." NSEC
        length (additional ans) `shouldBe` 4
        additional ans `shouldSatisfy` include "ai.example." A
        additional ans `shouldSatisfy` includeRRSIG "ai.example." A
        additional ans `shouldSatisfy` include "ai.example." AAAA
        additional ans `shouldSatisfy` includeRRSIG "ai.example." AAAA
        flags ans `shouldSatisfy` authAnswer
    it "passes the test in Appendix B.7 (Wildcard No Data Error)" $ do
        -- Sec 3.1.3.4.  Including NSEC RRs: Wildcard No Data Response
        let query = dnssecQuery{question = Question "a.z.w.example." AAAA IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 6
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "x.y.w.example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "x.y.w.example." NSEC
        authority ans `shouldSatisfy` include "*.w.example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "*.w.example." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "passes the test in Appendix B.8 (DS Child Zone No Data Error)" $ do
        -- Sec 3.1.4.1.  Responding to Queries for DS RRs
        let query = dnssecQuery{question = Question "example." DS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example." SOA
        authority ans `shouldSatisfy` includeRRSIG "example." SOA
        authority ans `shouldSatisfy` include "example." NSEC
        authority ans `shouldSatisfy` includeRRSIG "example." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

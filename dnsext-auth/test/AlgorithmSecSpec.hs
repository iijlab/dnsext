{-# LANGUAGE OverloadedStrings #-}

module AlgorithmSecSpec where

import Test.Hspec

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types

import Data.Maybe

spec :: Spec
spec = describe "authoritative algorithm" $ do
    runIO $ runInitIO $ addResourceDataForDNSSEC
    let zone = "example.jp."
    edb <- runIO $ do
        rrs <- loadZoneFile zone "test/example.zone"
        (_pub, _pri, dnskey, _ds, doSign) <-
            prepareDNSSEC $
                DNSSECinfo
                    { dnssecInfoZone = zone
                    , dnssecInfoPubAlg = ED25519
                    , dnssecInfoDigestAlg = SHA256
                    , dnssecInfoTTL = 3600
                    , dnssecInfoDuration = 86400
                    }
        makeDBforPrimary zone doSign (rrs ++ [dnskey])
    let db = fromJust edb
    doit db
    db2 <- fromJust <$> runIO (makeDBforSecondary zone $ dbAll db)
    doit db2

-- Canonical order:
-- example.jp.
-- -- *.example.jp.
-- b.example.jp.
-- -- ent1.example.jp.
-- -- ent2.ent1.example.jp.
-- a.ent2.ent1.example.jp.
-- exist.example.jp.
-- exist-cname.example.jp.
-- ext-cname.example.jp.
-- fault-cname.example.jp.
-- in.example.jp.
-- in2.example.jp.
-- -- nonexist.example.jp.
-- in-cname.example.jp.
-- ns.example.jp.
-- sibling.example.jp.
-- -- ns.sibling.example.jp.
-- sibling2.example.jp.
-- -- ns.sibling2.example.jp.

doit :: DB -> Spec
doit db = do
    it "can answer an existing domain" $ do
        let query = dnssecQuery{question = Question "exist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "exist.example.jp." A
        answer ans `shouldSatisfy` includeRRSIG "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer a non-existing domain" $ do
        let query = dnssecQuery{question = Question "nonexist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 6
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        authority ans `shouldSatisfy` include "in2.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in2.example.jp." NSEC
        authority ans `shouldSatisfy` include "example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "example.jp." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can refuse unrelated domains" $ do
        let query = dnssecQuery{question = Question "unrelated.com." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` Refused
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (1)" $ do
        let query = dnssecQuery{question = Question "in.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 5
        authority ans `shouldSatisfy` includeNS "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated.com."
        authority ans `shouldSatisfy` include "in.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in.example.jp." NSEC
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (2)" $ do
        let query = dnssecQuery{question = Question "in2.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 5
        authority ans `shouldSatisfy` includeNS "ns.in2.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling2.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated2.com."
        authority ans `shouldSatisfy` include "in2.example.jp." DS
        authority ans `shouldSatisfy` includeRRSIG "in2.example.jp." DS
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in2.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling2.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated2.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (3)" $ do
        let query = dnssecQuery{question = Question "foo.in.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 5
        authority ans `shouldSatisfy` includeNS "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated.com."
        authority ans `shouldSatisfy` include "in.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in.example.jp." NSEC
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (4)" $ do
        let query = dnssecQuery{question = Question "foo.in2.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 5
        authority ans `shouldSatisfy` includeNS "ns.in2.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling2.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated2.com."
        authority ans `shouldSatisfy` include "in2.example.jp." DS
        authority ans `shouldSatisfy` includeRRSIG "in2.example.jp." DS
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in2.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling2.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated2.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer DS not referrals" $ do
        let query = dnssecQuery{question = Question "in2.example.jp." DS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "in2.example.jp." DS
        answer ans `shouldSatisfy` includeRRSIG "in2.example.jp." DS
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer referrals via NS" $ do
        let query = dnssecQuery{question = Question "ns.in.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 5
        authority ans `shouldSatisfy` includeNS "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated.com."
        authority ans `shouldSatisfy` include "in.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in.example.jp." NSEC
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "returns AA for NS of this domain" $ do
        let query = dnssecQuery{question = Question "example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` includeNS "ns.example.jp."
        answer ans `shouldSatisfy` includeRRSIG "example.jp." NS
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.example.jp." A
        additional ans `shouldSatisfy` includeRRSIG "ns.example.jp." A
        flags ans `shouldSatisfy` authAnswer
    it "returns a single minimum RR for ANY" $ do
        let query = dnssecQuery{question = Question "exist.example.jp." ANY IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "exist.example.jp." A
        answer ans `shouldSatisfy` includeRRSIG "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle existing CNAME" $ do
        let query = dnssecQuery{question = Question "exist-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 4
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        answer ans `shouldSatisfy` includeRRSIG "exist-cname.example.jp." CNAME
        answer ans `shouldSatisfy` include "exist.example.jp." A
        answer ans `shouldSatisfy` includeRRSIG "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle no-data CNAME" $ do
        let query = dnssecQuery{question = Question "exist-cname.example.jp." TXT IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        answer ans `shouldSatisfy` includeRRSIG "exist-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 2
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle nx-domain CNAME" $ do
        let query = dnssecQuery{question = Question "fault-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "fault-cname.example.jp." CNAME
        answer ans `shouldSatisfy` includeRRSIG "fault-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 6
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        authority ans `shouldSatisfy` include "in2.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in2.example.jp." NSEC
        authority ans `shouldSatisfy` include "example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "example.jp." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle unrelated CNAME" $ do
        let query = dnssecQuery{question = Question "ext-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "ext-cname.example.jp." CNAME
        answer ans `shouldSatisfy` includeRRSIG "ext-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle delegated CNAME" $ do
        let query = dnssecQuery{question = Question "in-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "in-cname.example.jp." CNAME
        answer ans `shouldSatisfy` includeRRSIG "in-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 5
        authority ans `shouldSatisfy` includeNS "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated.com."
        authority ans `shouldSatisfy` include "in-cname.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in-cname.example.jp." NSEC
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can handle existing CNAME for CNAME query" $ do
        let query = dnssecQuery{question = Question "exist-cname.example.jp." CNAME IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        answer ans `shouldSatisfy` includeRRSIG "exist-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 4
        additional ans `shouldSatisfy` include "exist.example.jp." A
        additional ans `shouldSatisfy` includeRRSIG "exist.example.jp." A
        additional ans `shouldSatisfy` include "exist.example.jp." AAAA
        additional ans `shouldSatisfy` includeRRSIG "exist.example.jp." AAAA
        flags ans `shouldSatisfy` authAnswer
    it "can handle Empty Non-Terminal node" $ do
        let query = dnssecQuery{question = Question "ent1.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        authority ans `shouldSatisfy` include "b.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "b.example.jp." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle Empty Non-Terminal node nested" $ do
        let query = dnssecQuery{question = Question "ent2.ent1.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        authority ans `shouldSatisfy` include "b.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "b.example.jp." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

    it "can answer an existing domain for NSEC" $ do
        let query = dnssecQuery{question = Question "exist.example.jp." NSEC IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "exist.example.jp." NSEC
        answer ans `shouldSatisfy` includeRRSIG "exist.example.jp." NSEC
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer a non-existing domain for NSEC" $ do
        let query = dnssecQuery{question = Question "nonexist.example.jp." NSEC IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 6
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        authority ans `shouldSatisfy` include "in2.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "in2.example.jp." NSEC
        authority ans `shouldSatisfy` include "example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "example.jp." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle Empty Non-Terminal node for NSEC" $ do
        let query = dnssecQuery{question = Question "ent1.example.jp." NSEC IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 4
        authority ans `shouldSatisfy` include "example.jp." SOA
        authority ans `shouldSatisfy` includeRRSIG "example.jp." SOA
        authority ans `shouldSatisfy` include "b.example.jp." NSEC
        authority ans `shouldSatisfy` includeRRSIG "b.example.jp." NSEC
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

includeRRSIG :: Domain -> TYPE -> [ResourceRecord] -> Bool
includeRRSIG dom typ rs = any has rs
  where
    has r =
        rrname r == dom && rrtype r == RRSIG && case fromRData $ rdata r of
            Nothing -> False
            Just rd -> rrsig_type rd == typ

includeNS :: Domain -> [ResourceRecord] -> Bool
includeNS dom rs = any has rs
  where
    has r = case fromRData $ rdata r of
        Nothing -> False
        Just rd -> ns_domain rd == dom

include :: Domain -> TYPE -> [ResourceRecord] -> Bool
include dom typ rs = any has rs
  where
    has r = rrname r == dom && rrtype r == typ

dnssecQuery :: DNSMessage
dnssecQuery =
    defaultQuery
        { ednsHeader = EDNSheader defaultEDNS{ednsDnssecOk = True}
        }

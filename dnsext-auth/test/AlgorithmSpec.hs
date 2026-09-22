{-# LANGUAGE OverloadedStrings #-}

module AlgorithmSpec where

import Test.Hspec

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.SEC
import DNS.Types

import Common

spec :: Spec
spec = describe "authoritative algorithm" $ do
    let zone = "example.jp."
    runIO $ runInitIO $ addResourceDataForDNSSEC
    db <- runIO $ loadDB zone "test/example.zone"
    doit db
    db2 <- runIO (makeDBforSecondary Checked zone $ dbAll db)
    doit db2

doit :: DB -> Spec
doit db = do
    it "can answer an existing domain" $ do
        let query = defaultQuery{question = Question "exist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer a non-existing domain" $ do
        let query = defaultQuery{question = Question "nonexist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can refuse unrelated domains" $ do
        let query = defaultQuery{question = Question "unrelated.com." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` Refused
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (1)" $ do
        let query = defaultQuery{question = Question "in.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "unrelated.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (2)" $ do
        let query = defaultQuery{question = Question "in2.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "in2.example.jp." "ns.in2.example.jp."
        authority ans `shouldSatisfy` includeNS "in2.example.jp." "ns.sibling2.example.jp."
        authority ans `shouldSatisfy` includeNS "in2.example.jp." "unrelated2.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in2.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling2.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated2.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (3)" $ do
        let query = defaultQuery{question = Question "foo.in.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "unrelated.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals (4)" $ do
        let query = defaultQuery{question = Question "foo.in2.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "in2.example.jp." "ns.in2.example.jp."
        authority ans `shouldSatisfy` includeNS "in2.example.jp." "ns.sibling2.example.jp."
        authority ans `shouldSatisfy` includeNS "in2.example.jp." "unrelated2.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in2.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling2.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated2.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "can answer referrals via NS" $ do
        let query = defaultQuery{question = Question "ns.in.example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "unrelated.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` not . authAnswer
    it "returns AA for NS of this domain" $ do
        let query = defaultQuery{question = Question "example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` includeNS "example.jp." "ns.example.jp."
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 1
        additional ans `shouldSatisfy` include "ns.example.jp." A
        flags ans `shouldSatisfy` authAnswer
    it "returns a single minimum RR for ANY" $ do
        let query = defaultQuery{question = Question "exist.example.jp." ANY IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle existing CNAME" $ do
        let query = defaultQuery{question = Question "exist-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 2
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        answer ans `shouldSatisfy` include "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle no-data CNAME" $ do
        let query = defaultQuery{question = Question "exist-cname.example.jp." TXT IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle nx-domain CNAME" $ do
        let query = defaultQuery{question = Question "fault-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "fault-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle unrelated CNAME" $ do
        let query = defaultQuery{question = Question "ext-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "ext-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle delegated CNAME" $ do
        let query = defaultQuery{question = Question "in-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "in-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "in.example.jp." "unrelated.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        additional ans `shouldSatisfy` not . include "unrelated.com." A
        flags ans `shouldSatisfy` authAnswer
    it "can handle existing CNAME for CNAME query" $ do
        let query = defaultQuery{question = Question "exist-cname.example.jp." CNAME IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle Empty Non-Terminal node" $ do
        let query = defaultQuery{question = Question "ent1.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle Empty Non-Terminal node nested" $ do
        let query = defaultQuery{question = Question "ent2.ent1.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer an existing domain for NSEC" $ do
        let query = defaultQuery{question = Question "exist.example.jp." NSEC IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer a non-existing domain for NSEC" $ do
        let query = defaultQuery{question = Question "nonexist.example.jp." NSEC IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle Empty Non-Terminal node for NSEC x" $ do
        let query = defaultQuery{question = Question "ent1.example.jp." NSEC IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 1
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer

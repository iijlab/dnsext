{-# LANGUAGE OverloadedStrings #-}

module AlgorithmSpec where

import Test.Hspec

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.Types

spec :: Spec
spec = describe "authoritative algorithm" $ do
    edb <- runIO $ loadDB "example.jp." "test/example.zone"
    let db = case edb of
            Left _ -> error "DB"
            Right db' -> db'
    it "can answer an exiting domain" $ do
        let query = defaultQuery{question = Question "exist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        answer ans `shouldSatisfy` include "exist.example.jp." A
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can answer an non-exiting domain" $ do
        let query = defaultQuery{question = Question "nonexist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
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
    it "can answer referrals" $ do
        let query = defaultQuery{question = Question "foo.in.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 3
        authority ans `shouldSatisfy` includeNS "ns.in.example.jp."
        authority ans `shouldSatisfy` includeNS "ns.sibling.example.jp."
        authority ans `shouldSatisfy` includeNS "unrelated.com."
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "ns.in.example.jp." A
        additional ans `shouldSatisfy` include "ns.sibling.example.jp." A
        flags ans `shouldSatisfy` not . authAnswer
    it "returns AA for NS of this domain" $ do
        let query = defaultQuery{question = Question "example.jp." NS IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` includeNS "ns.example.jp."
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
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
        flags ans `shouldSatisfy` authAnswer
    it "can handle nx-domain CNAME" $ do
        let query = defaultQuery{question = Question "fault-cname.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "fault-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 0
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
    it "can handle existing CNAME for CNAME query" $ do
        let query = defaultQuery{question = Question "exist-cname.example.jp." CNAME IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "exist-cname.example.jp." CNAME
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 2
        additional ans `shouldSatisfy` include "exist.example.jp." A
        additional ans `shouldSatisfy` include "exist.example.jp." AAAA
        flags ans `shouldSatisfy` authAnswer

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

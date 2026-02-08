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
    it "can answer an non-exiting domain" $ do
        let query = defaultQuery{question = Question "nonexist.example.jp." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
        authority ans `shouldSatisfy` include "example.jp." SOA
        length (additional ans) `shouldBe` 0
    it "can refuse unrelated domains" $ do
        let query = defaultQuery{question = Question "unrelated.com." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` Refused
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 0
        length (additional ans) `shouldBe` 0
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

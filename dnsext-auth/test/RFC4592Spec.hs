{-# LANGUAGE OverloadedStrings #-}

module RFC4592Spec where

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
        rrs <- loadZoneFile zone "test/rfc4592.zone"
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

doit :: DB -> Spec
doit db = do
    it "implements Sec 2.2.1 (synthesized)" $ do
        let query = defaultQuery{question = Question "host3.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "host3.example." MX
    it "implements Sec 2.2.1 (synthesized)" $ do
        let query = defaultQuery{question = Question "host3.example." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
    it "implements Sec 2.2.1 (synthesized)" $ do
        let query = defaultQuery{question = Question "foo.bar.example." TXT IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "foo.bar.example." TXT
    it "implements Sec 2.2.1 (not synthesized)" $ do
        let query = defaultQuery{question = Question "host1.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
    it "implements Sec 2.2.1 (not synthesized)" $ do
        let query = defaultQuery{question = Question "sub.*.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
    it "implements Sec 2.2.1 (not synthesized)" $ do
        let query = defaultQuery{question = Question "_telnet._tcp.host1.example." SRV IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
    it "implements Sec 2.2.1 (not synthesized)" $ do
        let query = defaultQuery{question = Question "host.subdel.example." A IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 0
        length (authority ans) `shouldBe` 2
    it "implements Sec 2.2.1 (not synthesized)" $ do
        let query = defaultQuery{question = Question "ghost.*.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
    it "implements Sec 3.3.2" $ do
        let query = defaultQuery{question = Question "host3.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "host3.example." MX
    it "implements Sec 3.3.2" $ do
        let query = defaultQuery{question = Question "_telnet._tcp.host1.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
    it "implements Sec 3.3.2" $ do
        let query = defaultQuery{question = Question "_dns._udp.host2.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0
    it "implements Sec 3.3.2" $ do
        let query = defaultQuery{question = Question "_telnet._tcp.host3.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "_telnet._tcp.host3.example." MX
    it "implements Sec 3.3.2" $ do
        let query = defaultQuery{question = Question "_chat._udp.host3.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NoErr
        length (answer ans) `shouldBe` 1
        answer ans `shouldSatisfy` include "_chat._udp.host3.example." MX
    it "implements Sec 3.3.2" $ do
        let query = defaultQuery{question = Question "foobar.*.example." MX IN}
            ans = getAnswer db query
        rcode ans `shouldBe` NXDomain
        length (answer ans) `shouldBe` 0

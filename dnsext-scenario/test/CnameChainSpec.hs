{-# LANGUAGE OverloadedStrings #-}

-- | How far bowline follows a chain of CNAMEs.
--
--   RFC 1034 Sec 3.6.2 says a resolver must not loop for ever and
--   leaves the rest to it, so every resolver has a limit of its own and
--   c1-c30.internot.jp is where they are compared.  bowline's is
--   maxCNameChain in DNS.Iterative.Query.Resolve, and this is where it
--   is written down from the outside: a chain shorter than the limit is
--   followed to the end, and one longer than it is refused rather than
--   answered halfway.
--
--   See https://www.e-ontap.com/dns/samples.html
module CnameChainSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "cname-chain") $
    describe "a chain of CNAMEs" $ do
        it "is followed to the end when it is short enough" $ \sc -> do
            a <- ask sc "c25.example." A
            answerRcode a `shouldBe` NoErr
            rdatasOf A a `shouldBe` [rd_a "192.0.2.30"]

        -- Every link of it, not just the last: c25 to c30 is six of
        -- them, and then the name at the end.
        it "carries every link of it in the answer" $ \sc -> do
            a <- ask sc "c25.example." A
            length (rdatasOf CNAME a) `shouldBe` 6

        -- Sixteen links is what bowline will follow -- maxCNameChain in
        -- DNS.Iterative.Query.Resolve -- so c15, which is sixteen of
        -- them, is the longest chain it answers.
        it "is followed to sixteen links" $ \sc -> do
            a <- ask sc "c15.example." A
            answerRcode a `shouldBe` NoErr
            rdatasOf A a `shouldBe` [rd_a "192.0.2.30"]

        it "is refused rather than answered halfway at seventeen" $ \sc -> do
            a <- ask sc "c14.example." A
            answerRcode a `shouldBe` ServFail
            answerRRs a `shouldBe` []

        it "is refused however much too long it is" $ \sc -> do
            a <- ask sc "c1.example." A
            answerRcode a `shouldBe` ServFail
            answerRRs a `shouldBe` []

-- | What the answer says of the given type.  A question asked with DO
--   set brings the RRSIGs back as well, and they are not what a
--   scenario is looking at.
rdatasOf :: TYPE -> Answer -> [RData]
rdatasOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

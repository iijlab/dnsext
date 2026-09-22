{-# LANGUAGE OverloadedStrings #-}

-- | What bowline makes of a zone it is supposed to validate and cannot.
--
--   The parent publishes a DS for the child, so the delegation is a
--   secure one and the child's data is the resolver's business; the DS
--   is for a key the child does not have, so nothing the child sends
--   can be reached from the trust anchor.  That is
--   dnssec-failed.mufj.jp, and the answer is not in doubt -- RFC 4035
--   Sec 5.5 has a validator refuse data it cannot authenticate -- but
--   it is the other side of the jp.sharp scenario, and what says that
--   one is about an insecure delegation rather than about bowline not
--   validating at all.
--
--   See https://www.e-ontap.com/dns/samples.html
module DnssecFailedSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "dnssec-failed") $
    describe "a zone whose DS is for a key it does not have" $ do
        it "validates the parent, which is signed and vouched for" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True

        it "refuses what it cannot authenticate" $ \sc -> do
            a <- ask sc "host.bogus.example." A
            answerRcode a `shouldBe` ServFail
            answerRRs a `shouldBe` []

        -- RFC 4035 Sec 3.2.2: with CD set the resolver hands over what
        -- it has without validating it, for a client which would rather
        -- check for itself.
        it "hands it over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "host.bogus.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` False
            map rdata (answerRRs a) `shouldBe` [rd_a "192.0.2.9"]

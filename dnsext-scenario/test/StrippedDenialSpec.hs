{-# LANGUAGE OverloadedStrings #-}

-- | A signed zone which strips the NSEC3s out of its referrals, so
--   that a delegation arrives with nothing to say whether the child has
--   a DS.  RFC 4035 Sec 5.2 wants an authenticated denial before a
--   child is taken as unsigned.
module StrippedDenialSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "stripped-denial") $
    describe "a referral which says nothing about the DS" $ do
        -- No DS, and no proof that there is none.
        it "is not a reason to stop validating" $ \sc -> do
            a <- ask sc "sub.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            answerRRs a `shouldBe` []

        -- CD means the querier will check for itself.
        it "hands the child over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "sub.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.80"]

        -- Only the NSEC3s are gone, so the zone itself still validates:
        -- the refusal above is about the delegation, not the zone.
        it "leaves the zone which sent it validating as before" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

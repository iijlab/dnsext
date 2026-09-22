{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with a referral which proves nothing.
--
--   A signed zone which delegates a child it does not vouch for says so
--   in records it signs: an NSEC or NSEC3 denying the DS.  That denial
--   is the whole of a resolver's reason to stop validating below the
--   delegation, and RFC 4035 Sec 5.2 has it conclude that a child is
--   unsigned only from an /authenticated/ denial of the DS RRset.
--
--   Here @example.@ leaves every NSEC3 out of what it sends.  The
--   referral to @sub.example.@ carries the delegation and nothing at
--   all about whether the child has a DS -- which is what is left of a
--   message after somebody who can drop records from it has been
--   through, and dropping records is the attack DNSSEC exists to
--   answer.
--
--   The root vouches for @example.@, so bowline has its key and knows
--   it is signed.  An unsigned parent is a different matter: there is
--   no denial to expect from one, and nothing here asks for one.
module StrippedDenialSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "stripped-denial") $
    describe "a referral which says nothing about the DS" $ do
        -- There is no DS and no proof that there is none.  The second
        -- of those is what makes a delegation insecure, and it did not
        -- arrive.
        it "is not a reason to stop validating" $ \sc -> do
            a <- ask sc "sub.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            answerRRs a `shouldBe` []

        -- With CD the querier has said it will check for itself, and
        -- gets the child's data as it came.
        it "hands the child over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "sub.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.80"]

        -- Only the NSEC3s are missing, so the zone itself still signs
        -- everything it answers with and still validates.  The refusal
        -- above is about the delegation and not about the zone.
        it "leaves the zone which sent it validating as before" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

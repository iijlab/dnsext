{-# LANGUAGE OverloadedStrings #-}

-- | A root which will not prime.  It adds a name server of its own to
--   every answer, so the NS RRset which arrives is not the one its
--   RRSIG covers and the priming query does not verify.
module PrimingFailedSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "priming-failed") $
    describe "a root which will not prime" $ do
        -- The NS RRset which arrives is not the one the root signed.
        -- Before, it came back as if it were the root's own.
        it "has the name servers it will not vouch for refused" $ \sc -> do
            a <- ask sc "." NS
            answerRcode a `shouldBe` ServFail
            answerRRs a `shouldBe` []

        -- The root's key was verified against the trust anchor before
        -- priming, and its DS for example. is good.
        it "still leaves the zones below it validated" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            [rdata rr | rr <- answerRRs a, rrtype rr == A] `shouldBe` [rd_a "192.0.2.1"]

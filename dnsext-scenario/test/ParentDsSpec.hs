{-# LANGUAGE OverloadedStrings #-}

-- | A DS lives on the parent's side of a delegation and never at an
--   apex (RFC 4034 Sec 5, RFC 4035 Sec 2.4), so the child is the one
--   server certain not to have it.  The examples run in the order
--   written, on one cache, and the first is the first thing asked.
module ParentDsSpec (spec) where

import DNS.SEC
import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "parent-ds") $
    describe "the DS of a delegation" $ do
        -- The root vouched with a DS, so that is what should come back.
        it "is the one the parent vouched with" $ \sc -> do
            a <- ask sc "example." DS
            rdataOf DS a `shouldSatisfy` not . null
            answerAuthentic a `shouldBe` True

        -- Asking the question above must not cost the zone its chain.
        -- Only means anything after it, on a cache empty before it.
        it "does not leave the zone below it unvalidated" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- No DS to give, proved in records example. signed.
        it "is nothing, provably, where the parent vouched for nobody" $ \sc -> do
            a <- ask sc "up.example." DS
            answerRcode a `shouldBe` NoErr
            rdataOf DS a `shouldBe` []

        -- The same where the child's servers are not answering: the
        -- parent signed the denial, so they are not needed.
        it "is nothing where the delegation leads nowhere" $ \sc -> do
            a <- ask sc "lame.example." DS
            answerRcode a `shouldBe` NoErr
            rdataOf DS a `shouldBe` []

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

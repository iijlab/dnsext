{-# LANGUAGE OverloadedStrings #-}

-- | A chain of two delegations, walked across two servers.
--
--   The root delegates @example.@ to the second address and vouches for
--   it with a DS; @example.@ delegates @sub.example.@ back to the first
--   and carries no DS, so everything below that second cut is insecure.
--   @sub.example.@ signs itself anyway, which nothing forbids and which
--   nobody has any reason to look at.
--
--   The first address therefore holds the root and the root's
--   grandchild without holding what is in between.  That is the only
--   arrangement which gets three levels out of two servers, and two is
--   what one loopback interface has.
module DeepDelegationSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "deep-delegation") $
    describe "two delegations, one of them insecure" $ do
        it "validates the middle zone, which the root vouches for" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        it "answers below the second cut without saying it validated" $ \sc -> do
            a <- ask sc "www.sub.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` False
            rdataOf A a `shouldBe` [rd_a "192.0.2.9"]

        -- Both cuts were walked, and walking them crossed from one
        -- address to the other and back: the root gave the first
        -- delegation, the second address gave the second, and the
        -- answer came from the first again.
        it "walked both cuts, crossing between the two servers" $ \sc -> do
            _ <- ask sc "www.sub.example." A
            root <- asked sc TheRoot
            prim <- asked sc ThePrimary
            root `shouldSatisfy` elem ("example.", A)
            prim `shouldSatisfy` elem ("sub.example.", A)
            root `shouldSatisfy` elem ("www.sub.example.", A)

rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

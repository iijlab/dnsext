{-# LANGUAGE OverloadedStrings #-}

-- | What bowline makes of a delegation with no DS whose server holds the
--   parent's apex rather than the child's.
--
--   jp.sharp did that in 2020: @sharp.@ delegated @jp.sharp.@ with no DS,
--   and the server it pointed at answered from a @sharp.@ of its own,
--   signed with a key the real @sharp.@ does not have.  Here the root
--   delegates @insecure.@ the same way, and the primary answers from a
--   root zone of its own.  "JpSharpSpec" is insecure.mufj.jp, where only
--   the signer field names another zone.
--
--   See https://www.e-ontap.com/dns/amagasaki2020-neg/
module JpSharp2020Spec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "jp.sharp-2020") $
    describe "a delegation with no DS whose server holds the parent's apex" $ do
        -- The world is worth nothing if the part of it which is meant
        -- to validate does not.
        it "validates what the root vouches for" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True

        -- No DS, so nothing below the delegation is validated, and
        -- whose apex the answer comes from is not looked at either.
        it "answers from the server it was delegated to, unvalidated" $ \sc -> do
            a <- ask sc "host.insecure." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` False
            map rdata (answerRRs a) `shouldBe` [rd_a "192.0.2.10"]

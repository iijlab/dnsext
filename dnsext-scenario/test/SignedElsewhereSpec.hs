{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with signatures which name a zone that did not
--   make them, where the parent has said to check.
--
--   The zone is signed with its own key and every RRSIG over it is then
--   made to say @elsewhere.@ in the signer field.  The data is right,
--   the key is right, the DS in the root is right: what is wrong is
--   that no key a resolver can reach will verify the signatures, the
--   zone they name not being the zone that made them.
--
--   "JpSharpSpec" is the same fault with the parent saying nothing.
--   There the delegation carries no DS, so the child is insecure and
--   its signatures are nobody's business -- bowline hands the answer
--   over without claiming to have checked it.  Here the parent carries
--   a DS, so they are very much its business.
--
--   The pair is what the fake.ed448.mufj.jp family is about: a signed
--   zone whose signatures do not hold up.  Which of the two a resolver
--   is looking at is the difference between an answer and a refusal.
--
--   See https://www.e-ontap.com/dns/samples.html
module SignedElsewhereSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "signed-elsewhere") $
    describe "signatures naming a zone which did not make them" $ do
        -- The parent vouched, so the signatures have to hold, and they
        -- do not.  Nothing is handed over.
        it "refuses what it was told to check and cannot" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            answerRRs a `shouldBe` []

        -- With CD set, the data itself turns out to be perfectly good:
        -- what was wrong was only the name in the signer field.
        it "hands the data over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "www.example." A
            answerRcode a `shouldBe` NoErr
            [rdata rr | rr <- answerRRs a, rrtype rr == A] `shouldBe` [rd_a "192.0.2.1"]

        -- And the chain above is whole, so the refusal is about this
        -- zone and not about the world it hangs from.
        it "still validates the zone above" $ \sc -> do
            a <- ask sc "kept." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True

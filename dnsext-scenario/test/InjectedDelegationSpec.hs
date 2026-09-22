{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does when a server hands it a delegation for a zone
--   that server has nothing to do with.
--
--   The root here keeps @www.victim.@ for itself: it is ordinary
--   root-zone data, signed by the root, and there is no @victim.@
--   delegation anywhere.  @example.@ is delegated away as usual, and
--   the server it is delegated to attaches to every response a
--   delegation for @victim.@ and the glue to go with it, pointing at a
--   server which really will answer for @victim.@ and answer
--   differently.
--
--   Nothing about that is @example.@'s to say, and a resolver which
--   takes it moves every later question about @victim.@ off the root
--   and onto whoever the injection names.  That is what a move
--   injection is, and flip.e-ontap.com is the way to check for one.
--
--   See https://www.e-ontap.com/dns/samples.html
module InjectedDelegationSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "injected-delegation") $
    describe "a delegation from a server which is not the parent" $ do
        -- The question being asked when the injection arrives.
        it "answers the question it was asked" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- And then the name the injection was about.  The root's answer
        -- is the true one, and the injected server's is not reached.
        it "keeps asking the root, which is where the name really is" $ \sc -> do
            _ <- ask sc "www.example." A
            a <- ask sc "www.victim." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "203.0.113.66"]
            answerAuthentic a `shouldBe` True

        -- With CD set bowline does not validate.  The injection is
        -- turned down all the same, so what turns it down is not the
        -- signatures: a zone with none would be no worse off.
        it "turns it down without validating" $ \sc -> do
            _ <- askChecking sc "www.example." A
            a <- askChecking sc "www.victim." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "203.0.113.66"]

-- | What the answer says of one type.  With DO set a validated answer
--   carries its RRSIG too, which is not what any of these are about.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

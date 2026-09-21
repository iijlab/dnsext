{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does when a referral carries an address nobody asked
--   for and nobody is authoritative for.
--
--   The root of this world delegates both @example.@ and @victim.@
--   away, and then attaches to every response it sends an A record for
--   @www.victim.@ which is not the address @victim.@ gives.  That is
--   what clove's @spoof-additional@ is for, and it needs @--insecure@,
--   there being no honest use for it.
--
--   The additional section of a referral is where a resolver looks for
--   glue, and glue is never signed -- it is not the parent's data to
--   sign.  So there is nothing here for a validator to catch.  What
--   keeps the lie out is bailiwick alone: the root gave @victim.@ away,
--   so it is not the place to learn what is in it.
--
--   small-is-beautiful.jp is "この委任応答には毒が入れられるかも" and
--   hoge.sub.mufj.jp is "これで毒が入る実装があった".
--
--   See https://www.e-ontap.com/dns/samples.html
module PoisonedGlueSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "poisoned-glue") $
    describe "a referral which carries an address for another zone" $ do
        -- The question being asked when the lie arrives.  The referral
        -- for example. is what carries it.
        it "answers the question it was asked" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- And then the name the lie was about.  bowline goes and asks
        -- the zone, which is the only place that could tell it.
        it "asks the zone rather than believing what it was handed" $ \sc -> do
            _ <- ask sc "www.example." A
            a <- ask sc "www.victim." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "198.51.100.1"]
            answerAuthentic a `shouldBe` True

        -- With CD set bowline does not validate, so whatever keeps the
        -- lie out here is not DNSSEC.  It is bailiwick, and it has to
        -- be: an unsigned zone would get no other protection, and glue
        -- is unsigned even when everything around it is.
        it "keeps it out without validating, which is the only defence there is" $ \sc -> do
            _ <- askChecking sc "www.example." A
            a <- askChecking sc "www.victim." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "198.51.100.1"]

-- | What the answer says of one type.  With DO set a validated answer
--   carries its RRSIG too, which is not what any of these are about.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

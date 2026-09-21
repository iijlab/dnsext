{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does when the header denies a name and the proof below
--   it says the name is there.
--
--   @www.example.@ is in the zone with an A on it.  Asked for an AAAA,
--   the answer it is owed is NOERROR with nothing in it and -- the zone
--   being signed -- an NSEC saying which types the name does have.  This
--   server sends the NSEC and then denies the name in the header.
--
--   So the reply says two things.  Only one of them is signed, and it is
--   not the one in the header: an rcode is not covered by anything.  A
--   validator has the zone's own word that the name exists and the
--   server's unsigned word that it does not.
--
--   "BrokenEntSpec" is the same fault in an unsigned zone, where there
--   is nothing to contradict and the denial simply stands.
--
--   www.is.aist.go.jp is an NXDOMAIN where NODATA was owed, with
--   validation trouble to go with it.
--
--   See https://www.e-ontap.com/dns/samples.html
module DeniedNodataSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "denied-nodata") $
    describe "a denial contradicted by the proof it arrives with" $ do
        -- Nothing here can be believed, so nothing is handed over.
        it "refuses what it cannot make sense of" $ \sc -> do
            a <- ask sc "www.example." AAAA
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- With CD set the querier has said it will do its own checking,
        -- so it gets the reply as it came and can see the contradiction
        -- for itself.
        it "hands it over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "www.example." AAAA
            answerRcode a `shouldBe` NXDomain

        -- The server is otherwise sound and the zone otherwise
        -- validates: the fault is one name wide.
        it "is still the same zone everywhere else" $ \sc -> do
            a <- ask sc "other.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            [rdata rr | rr <- answerRRs a, rrtype rr == A] `shouldBe` [rd_a "192.0.2.2"]

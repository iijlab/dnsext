{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with data a signed zone sends unsigned.
--
--   The root vouches for @example.@ with a DS and the zone is signed,
--   so a resolver knows every RRset which comes out of it is supposed
--   to arrive with an RRSIG over it.  One does not: @fake.example.@ is
--   not in the zone at all, and what answers for it is attached to
--   every message the zone sends and signed by nothing -- there was
--   nothing to sign it, the zone having been signed before it was
--   added.
--
--   So the message says two things at once, and only one of them is
--   signed: the denial says the name is not there, and the record in
--   the answer says here it is.
--
--   fake.ed448.mufj.jp and its neighbours are where a signed zone is
--   wrong about its signatures -- missing them, forging them, or naming
--   an algorithm which does not match.  This is the first of the three;
--   the other two want an RRSIG written out in a zone file, which the
--   parser cannot read.
--
--   See https://www.e-ontap.com/dns/samples.html
module UnsignedRecordSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "unsigned-record") $
    describe "data a signed zone sends unsigned" $ do
        -- RFC 4035 Sec 5.3: an RRset in a signed zone with no signature
        -- over it cannot be authenticated, and an answer which cannot
        -- be authenticated is not handed on.
        it "refuses it" $ \sc -> do
            a <- ask sc "fake.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            answerRRs a `shouldBe` []

        -- With CD set the querier has said it will do its own checking,
        -- so it gets the message as it came and can see the two halves
        -- disagree for itself.
        it "hands it over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "fake.example." A
            answerRcode a `shouldBe` NXDomain
            rdataOf A a `shouldBe` [rd_a "192.0.2.99"]

        -- And the zone is otherwise whole: the fault is one name wide.
        it "is still the same signed zone everywhere else" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

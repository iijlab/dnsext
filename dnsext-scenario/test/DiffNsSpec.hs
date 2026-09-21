{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does when a parent and a child do not name the same
--   name servers.
--
--   The referral says @example. NS ns-parent.example.@ and carries the
--   glue for it.  The child's own apex says @example. NS
--   ns-child.example.@ and carries an address for that.  The two RRsets
--   have nothing in common but the zone they are about.
--
--   Neither half is wrong on its own, and both names lead to the server
--   which really does hold the zone, so nothing here stops anyone
--   reaching it.  The protocol nowhere says the two must agree -- the
--   child's copy is the authoritative one and the parent's is a
--   pointer -- and in practice they drift apart whenever a zone moves
--   and one half of the change is forgotten.  Resolvers have
--   nevertheless made SERVFAIL of it.
--
--   www.diffns.internot.jp is this, and it is said to be where "親子の
--   名前の相違が BIND で SERVFAIL に至る".  bowline answers.
--
--   See https://www.e-ontap.com/dns/samples.html
module DiffNsSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "diff-ns") $
    describe "a parent and a child which name different name servers" $ do
        -- The disagreement is not itself a reason to refuse.
        it "answers, and validates the answer" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.2"]

        -- Asked which servers hold the zone, bowline gives the child's
        -- answer rather than the parent's.  That is the RRset with
        -- something to be authoritative about: the parent's is a
        -- pointer, and is not signed by the parent either.
        it "gives the child's name servers, not the parent's" $ \sc -> do
            a <- ask sc "example." NS
            answerRcode a `shouldBe` NoErr
            rdataOf NS a `shouldBe` [rd_ns "ns-child.example."]

        -- And the disagreement really is total: the name the referral
        -- gave is not in the child at all, so the child denies it.
        -- bowline reached the zone through it all the same.
        it "denies the name the referral was made with" $ \sc -> do
            a <- ask sc "ns-parent.example." A
            answerRcode a `shouldBe` NXDomain

-- | What the answer says of one type.  With DO set a validated answer
--   carries its RRSIG too, which is not what any of these are about.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

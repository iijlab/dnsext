{-# LANGUAGE OverloadedStrings #-}

-- | Whose opinion of how long a delegation may be kept is the one that
--   counts.
--
--   A delegation exists twice over: in the parent, which hands it out
--   in a referral, and in the child, whose own apex NS RRset is the
--   authoritative copy.  The two carry their own TTLs and nothing makes
--   them agree.
--
--   It matters when a zone is about to move.  The way to prepare is to
--   shorten the zone's own NS TTL, wait, and then change the
--   delegation, so that everybody follows within the short time rather
--   than the long one.  That only works on a resolver which keeps the
--   zone's number.  d.t.e-ontap.com moves between two servers every
--   five minutes to find out who does: the delegation there has 420
--   seconds on it and the zone has 60, and 8.8.8.8 was found to take
--   the parent's, so a change reached it about seven minutes late.  The
--   author's point is that 「浸透が遅い」 is not something DNS does, it
--   is something a resolver does.
--
--   Here the two zones are set up opposite ways round, so that whichever
--   number bowline keeps, one of them sends it back to the root after
--   three seconds and the other does not.
--
--   See https://www.e-ontap.com/dns/samples.html and
--   http://www.e-ontap.com/dns/propagation/test/
module NsTtlSpec (spec) where

import Control.Concurrent (threadDelay)
import DNS.Types
import Test.Hspec

import Harness

-- | Longer than the two seconds either of the short TTLs has on it.
pastTheBriefOnes :: Int
pastTheBriefOnes = 3000000

spec :: Spec
spec = aroundAll (withScenario "ns-ttl") $
    describe "whose TTL a delegation is kept for" $ do
        it "keeps the delegation for as long as the parent said" $ \sc -> do
            a1 <- ask sc "www.parentlong." A
            b1 <- ask sc "www.parentbrief." A
            threadDelay pastTheBriefOnes
            a2 <- ask sc "other.parentlong." A
            b2 <- ask sc "other.parentbrief." A
            mapM_ (\a -> answerRcode a `shouldBe` NoErr) [a1, b1, a2, b2]
            root <- asked sc TheRoot
            let times n = length [() | (m, A) <- root, m == n]
            -- The parent says an hour and the zone says two seconds.
            -- The two seconds buy the zone nothing: bowline does not go
            -- back, and would have gone on using these servers for the
            -- rest of the hour.
            times "parentlong." `shouldBe` 1
            -- The other way round, and the parent's two seconds are
            -- honoured although the zone asked for an hour.
            times "parentbrief." `shouldSatisfy` (>= 2)

        -- Which follows from this: the authoritative copy of the NS
        -- RRset is never asked for.  bowline has only ever seen the
        -- parent's, so the parent's is the only TTL it could be
        -- keeping.
        it "never asks a zone for its own name servers" $ \sc -> do
            _ <- ask sc "www.parentlong." A
            prim <- asked sc ThePrimary
            [n | (n, NS) <- prim] `shouldBe` []

        -- Both delegations carry no DS, so what comes back from below
        -- them is not validated and nothing in the timing above turns
        -- on DNSSEC.  The root still has to be able to say there is no
        -- DS, which is a thing clove could not do when this scenario
        -- was written -- it put a DS on both to get round it.
        it "does not claim to have validated either zone" $ \sc -> do
            a <- ask sc "www.parentlong." A
            b <- ask sc "www.parentbrief." A
            answerAuthentic a `shouldBe` False
            answerAuthentic b `shouldBe` False

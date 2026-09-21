{-# LANGUAGE OverloadedStrings #-}

-- | How much of a name bowline tells each server it asks.
--
--   @a.b.c.example.@ is four labels below the root, and only the last
--   of them has any records: @c.example.@ and @b.c.example.@ exist only
--   because something below them does.  A resolver walking down to it
--   chooses, at each step, how much of the name to put in the question.
--
--   Putting all of it in every question is the old way and the easy
--   one, and it tells the root operator every name anybody looks up.
--   RFC 7816 says to send no more than the server being asked has to
--   know: the name of the zone it is being asked to delegate, one label
--   at a time.
--
--   This is the one thing about a scenario which cannot be seen in the
--   answer -- the answer is the same either way -- so it is read out of
--   what the servers logged.  elb.amazonaws.com carries a TXT record
--   which says whether the resolver asking for it minimised, which is
--   the same question asked from the other end.
--
--   See https://www.e-ontap.com/dns/samples.html
module QnameMinSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "qname-min") $
    describe "how much of the name each server is told" $ do
        -- Asking again costs the servers nothing once the answer is
        -- cached, so each of these may start from the question.
        it "answers" $ \sc -> do
            a <- ask sc "a.b.c.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            [rdata rr | rr <- answerRRs a, rrtype rr == A] `shouldBe` [rd_a "192.0.2.1"]

        -- The root is asked to delegate example. and is told nothing
        -- else.  It never hears the name the client asked for.
        it "tells the root only the zone it is asking it to delegate" $ \sc -> do
            _ <- ask sc "a.b.c.example." A
            qs <- asked sc TheRoot
            [n | (n, _) <- qs, n /= "."] `shouldBe` ["example."]

        -- And the primary is walked down a label at a time, rather than
        -- being handed the whole name at once.
        it "walks down the primary one label at a time" $ \sc -> do
            _ <- ask sc "a.b.c.example." A
            qs <- asked sc ThePrimary
            [n | (n, t) <- qs, t == A]
                `shouldBe` ["c.example.", "b.c.example.", "a.b.c.example."]

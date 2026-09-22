{-# LANGUAGE OverloadedStrings #-}

-- | What bowline answers when it is asked for a delegation's DS.
--
--   A DS belongs to the parent.  RFC 4034 Sec 5 has it appear only on
--   the parental side of a delegation and RFC 4035 Sec 2.4 has it not
--   appear at the zone's apex, so the zone itself is the one server
--   which is certain not to have it.
--
--   The root here delegates @example.@ and vouches for it with a DS, and
--   @example.@ delegates two children of its own and vouches for
--   neither.  One of those children answers and the other names a
--   server which is not there, which is the ordinary condition of an
--   insecure delegation nobody is looking after any more.
--
--   The examples below run in the order they are written, on one
--   resolver with one cache.  The first of them is the first thing this
--   bowline is asked, which matters: the DS of a zone is what its whole
--   subtree is validated from, and an answer to that question is kept.
module ParentDsSpec (spec) where

import DNS.SEC
import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "parent-ds") $
    describe "the DS of a delegation" $ do
        -- The root vouched for example. with a DS and bowline verified
        -- it on the way down.  Asked for it, that is what it should
        -- hand over.
        it "is the one the parent vouched with" $ \sc -> do
            a <- ask sc "example." DS
            rdataOf DS a `shouldSatisfy` not . null
            answerAuthentic a `shouldBe` True

        -- And being asked is not an event: the question above is an
        -- ordinary one, anybody may ask it, and the answer to it is
        -- what the zone below is validated from.  This example only
        -- means anything after that one, on a cache which was empty
        -- before it.
        it "does not leave the zone below it unvalidated" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- example. vouches for neither child, and says so in records it
        -- signs.  There is no DS to give, and that is an answer.
        it "is nothing, provably, where the parent vouched for nobody" $ \sc -> do
            a <- ask sc "up.example." DS
            answerRcode a `shouldBe` NoErr
            rdataOf DS a `shouldBe` []

        -- The same, for a delegation whose servers are not answering.
        -- Nothing about this question needs them: the parent signed the
        -- denial and bowline has the parent's key.
        it "is nothing where the delegation leads nowhere" $ \sc -> do
            a <- ask sc "lame.example." DS
            answerRcode a `shouldBe` NoErr
            rdataOf DS a `shouldBe` []

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

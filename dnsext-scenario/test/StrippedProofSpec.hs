{-# LANGUAGE OverloadedStrings #-}

-- | A signed zone which sends no denials.  @example.@ here signs
--   everything it holds and leaves every NSEC3 out of what it sends,
--   which is what a message looks like after records were dropped.
module StrippedProofSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "stripped-proof") $
    describe "a signed zone which sends no denials" $ do
        -- Only the NSEC3s are gone, so everything the zone actually
        -- holds still arrives signed and still validates.
        it "still answers for what it holds" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- RFC 4035 Sec 5.4: a name error from a signed zone is believed
        -- on the strength of the records which deny the name, and there
        -- are none.
        it "cannot be believed when it says a name is not there" $ \sc -> do
            a <- ask sc "absent.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- The same for a type which is not at a name which is.
        it "cannot be believed when it says a type is not there" $ \sc -> do
            a <- ask sc "www.example." TXT
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- RFC 4035 Sec 5.3.4 and RFC 5155 Sec 8.8: an answer whose
        -- RRSIG has fewer labels came from a wildcard, and is only this
        -- name's answer if nothing closer exists.
        it "cannot be believed when it answers from a wildcard" $ \sc -> do
            a <- ask sc "anything.wild.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- Refusing it once is not enough: verifying the RRset is what
        -- caches it, so every client after the refused one is handed
        -- the answer with AD set.
        it "cannot be believed the second time either" $ \sc -> do
            _ <- ask sc "twice.wild.example." A
            a <- ask sc "twice.wild.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            rdataOf A a `shouldBe` []

        -- The same answer from a zone which sends what it signs is
        -- taken.  intact. is example. again with the setting left off.
        it "takes a wildcard answer which came with the record for it" $ \sc -> do
            a <- ask sc "anything.wild.intact." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.8"]

        -- And keeps it: the witness is asked for before the RRset is
        -- verified, and verifying is what caches it, so a wildcard
        -- answer which has its record must still reach the cache.
        it "keeps a wildcard answer which came with the record for it" $ \sc -> do
            a <- ask sc "kept.wild.intact." A
            answerAuthentic a `shouldBe` True
            b <- ask sc "kept.wild.intact." A
            answerRcode b `shouldBe` NoErr
            answerAuthentic b `shouldBe` True
            rdataOf A b `shouldBe` [rd_a "192.0.2.8"]

        -- With CD the querier has said it will check for itself.
        it "hands what it has over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "anything.wild.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.7"]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

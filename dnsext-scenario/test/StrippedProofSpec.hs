{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with a signed zone which sends no denials.
--
--   @example.@ here signs everything it holds and leaves every NSEC3
--   out of what it sends.  Its own data still verifies; what is gone is
--   the records which say what is /not/ there.
--
--   That is what a message looks like after somebody who can drop
--   records from it has been through, and those records are what three
--   different answers rest on: a name error, a NODATA, and an answer
--   which came from a wildcard rather than from a name of its own.
--   RFC 4035 Sec 5.4 wants an authenticated denial before a name error
--   or a NODATA is believed, and Sec 5.3.4 -- RFC 5155 Sec 8.8 for
--   NSEC3 -- wants one before a wildcard answer is, since without it
--   any signed wildcard answer can be handed over for a name which has
--   a record of its own.
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
        -- RRSIG has fewer labels than the name it arrived under came
        -- from a wildcard, and is only that name's answer if nothing
        -- closer exists.  The record which would say so is missing, so
        -- this answer is any name's answer -- including one which has a
        -- record of its own that it would be overriding.
        it "cannot be believed when it answers from a wildcard" $ \sc -> do
            a <- ask sc "anything.wild.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- And the same answer from a zone which sends what it signs is
        -- taken, which is the point of asking for the record rather
        -- than of refusing wildcards.  intact. is example. again with
        -- the setting left off.
        it "takes a wildcard answer which came with the record for it" $ \sc -> do
            a <- ask sc "anything.wild.intact." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.8"]

        -- With CD the querier has said it will check for itself.
        it "hands what it has over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "anything.wild.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.7"]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

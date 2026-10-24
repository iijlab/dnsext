{-# LANGUAGE OverloadedStrings #-}

-- | A signed zone which leaves every NSEC3 out of what it sends.
module StrippedProofSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "stripped-proof") $
    describe "a signed zone which sends no denials" $ do
        -- Only the NSEC3s are gone, so what the zone holds still validates.
        it "still answers for what it holds" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- RFC 4035 Sec 5.4: no records deny the name.
        it "cannot be believed when it says a name is not there" $ \sc -> do
            a <- ask sc "absent.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- The same for a type which is not at a name which is.
        it "cannot be believed when it says a type is not there" $ \sc -> do
            a <- ask sc "www.example." TXT
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- RFC 4035 Sec 5.3.4, RFC 5155 Sec 8.8: no record says nothing closer exists.
        it "cannot be believed when it answers from a wildcard" $ \sc -> do
            a <- ask sc "anything.wild.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False

        -- Nor out of the cache on the second ask.
        it "cannot be believed the second time either" $ \sc -> do
            _ <- ask sc "twice.wild.example." A
            a <- ask sc "twice.wild.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            rdataOf A a `shouldBe` []

        -- intact. is example. again without spoof-strip.
        it "takes a wildcard answer which came with the record for it" $ \sc -> do
            a <- ask sc "anything.wild.intact." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.8"]

        -- And still caches it.
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

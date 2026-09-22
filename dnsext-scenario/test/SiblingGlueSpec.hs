{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with glue whose home is a third zone.
--
--   The root delegates @example.@ and @sibling.@, and @example.@'s only
--   name server is @ns.sibling.@ -- a name inside the other child.  The
--   root carries an address for it, because the name is below the
--   @sibling.@ delegation and that is what glue is; the same record is
--   then the only address there is for reaching @example.@ at all.
--
--   For @example.@ it is not glue in the strict sense: it is not below
--   @example.@, and @example.@ could not carry it if it wanted to.  The
--   one zone with any standing to say what address that host has is
--   @sibling.@, and here @sibling.@ says it has none.
--
--   So the only address in existence is the parent's copy, and whether
--   that may be passed on to a client as though the sibling zone had
--   said it is what tkix.net asks: "Sibling domain の glue は信じて良い
--   のでしょうか?".
--
--   See https://www.e-ontap.com/dns/samples.html
module SiblingGlueSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "sibling-glue") $
    describe "a zone whose name server is named inside a sibling zone" $ do
        -- Using it to get somewhere is what glue is for, and bowline
        -- does, which is the only way this zone is reachable at all.
        it "is reached on the address the referral carried" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- Handing it to a client is another matter.  Asked for the
        -- name, bowline gives what the zone which owns it says, and
        -- that zone says there is no address -- a signed no-such-data,
        -- the name being in the zone with other records on it.
        it "does not pass it on as the sibling zone's answer" $ \sc -> do
            _ <- ask sc "www.example." A
            a <- ask sc "ns.sibling." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` []
            answerAuthentic a `shouldBe` True

        -- The name is there; it is the address that is not.
        it "has the name itself, with what the sibling zone does say" $ \sc -> do
            a <- ask sc "ns.sibling." TXT
            answerRcode a `shouldBe` NoErr
            rdataOf TXT a
                `shouldBe` [rd_txt "the address of this host is nowhere in this zone"]

-- | What the answer says of one type.  With DO set a validated answer
--   carries its RRSIG too, which is not what any of these are about.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

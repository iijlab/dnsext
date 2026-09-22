{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with an address which rides along with the CNAME
--   that points at it.
--
--   @alias.example.@ is a CNAME for @www.elsewhere.@, and the two zones
--   are held by the same server.  A server in that position has the
--   target's address to hand and may send it along with the CNAME as a
--   kindness, saving the client a round trip; resolvers have taken it.
--
--   Here the address sent is not the one @elsewhere.@ gives, and
--   @example.@ has no standing to say anything about a name in another
--   zone whatever it holds.  In the answer section, beside the CNAME,
--   it looks exactly like the kindness.
--
--   f.uecac.jp is "CNAME に付随する A を信じてはいけない".
--
--   See https://www.e-ontap.com/dns/samples.html
module CnameExtraSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "cname-extra") $
    describe "an address sent along with the CNAME pointing at it" $ do
        -- The CNAME itself is example.'s to send, and is sent.
        it "follows the CNAME" $ \sc -> do
            a <- ask sc "alias.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf CNAME a `shouldBe` [rd_cname "www.elsewhere."]

        -- What comes back for the target is what the target's zone
        -- says, not what came free with the CNAME.
        it "takes the address from the zone which owns the name" $ \sc -> do
            a <- ask sc "alias.example." A
            rdataOf A a `shouldBe` [rd_a "198.51.100.7"]
            answerAuthentic a `shouldBe` True

        -- With CD set bowline does not validate, and the record still
        -- does not get in.  What keeps it out is that example. was
        -- never the place to ask, which is the only defence an unsigned
        -- zone would have had.
        it "keeps it out without validating" $ \sc -> do
            a <- askChecking sc "alias.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "198.51.100.7"]

        -- And nothing of it is left behind: asked for the name on its
        -- own, bowline gives the zone's answer too.
        it "did not keep it for the next question either" $ \sc -> do
            _ <- ask sc "alias.example." A
            a <- ask sc "www.elsewhere." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "198.51.100.7"]

-- | What the answer says of one type.  With DO set a validated answer
--   carries its RRSIG too, which is not what any of these are about.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

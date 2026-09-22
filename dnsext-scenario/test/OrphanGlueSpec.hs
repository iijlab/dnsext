{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does when the only address of a zone's name server is
--   the glue its parent carries.
--
--   The referral says @example. NS ns.example.@ and gives an address
--   for that name, because the name is below the delegation and no
--   other zone could.  The child says the same thing about its name
--   server and then says nothing more: there is no address record for
--   it anywhere in the child.
--
--   A resolver learns the NS RRset twice over -- once from the referral
--   and once, authoritatively and at a higher ranking, from the child
--   itself.  If taking the better copy means throwing the referral's
--   addresses away, there is nothing left to ask: the name to be looked
--   up can only be answered by the server being looked for.  So a zone
--   set up this way is reachable exactly as long as a resolver keeps
--   what the referral gave it.
--
--   rand.orphan.e-ontap.com is this, and the question it asks is
--   "親と子のどちらの NS を選ぶか?".
--
--   See https://www.e-ontap.com/dns/samples.html
module OrphanGlueSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "orphan-glue") $
    describe "a zone whose name server has an address only in its parent" $ do
        -- bowline keeps it, and goes on being able to ask.
        it "is reachable, on the address the referral gave" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- Keeping the parent's glue is not a way around DNSSEC: the
        -- answer still comes from the child, signed, under a DS the
        -- root vouches for.
        it "still validates what that address answers" $ \sc -> do
            a <- ask sc "www.example." A
            answerAuthentic a `shouldBe` True

        -- And the zone really is an orphan, more so than it had to
        -- be: the name it gives as its name server is not merely
        -- without an address there, it is not in the zone at all, and
        -- the child says so with a signed denial.  So the address
        -- bowline has been reaching it on all along belongs to a name
        -- which, authoritatively, does not exist.
        it "denies the very name it gives as its name server" $ \sc -> do
            a <- ask sc "ns.example." A
            answerRcode a `shouldBe` NXDomain
            rdataOf A a `shouldBe` []

-- | What the answer says of one type.  With DO set a validated answer
--   carries its RRSIG too, which is not what any of these are about.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

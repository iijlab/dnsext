{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with a signature naming an algorithm the zone has
--   no key for.
--
--   @example.@ is signed with an ED25519 key and has no other, so
--   ED25519 is the only algorithm anything in it can have been signed
--   with.  Attached to every answer it sends are two RRSIGs which say
--   algorithm 8, RSA\/SHA-256: they are written out in the zone file as
--   RFC 3597 Sec 5 generic RDATA, which is the only way to put fields
--   in an RRSIG that nothing would compute.  The root carries a DS for
--   the zone, so a resolver has been told to check.
--
--   The two are there for the two places such a signature can turn up.
--   @fake.example.@ is not in the zone, so the forged RRSIG is the only
--   signature it has and there is nothing else to fall back on.
--   @www.example.@ is in the zone and properly signed, so the forged
--   RRSIG arrives beside a good one -- and RFC 4035 Sec 5.3.3 asks only
--   that /one/ RRSIG over an RRset verify, so the good one should still
--   carry it.  A validator which insisted every signature hold would
--   let anyone break any RRset by adding one.
--
--   This is the last of the three faults the fake.ed448.mufj.jp family
--   is made of.  "UnsignedRecordSpec" is a signed zone sending data
--   with no signature at all, "SignedElsewhereSpec" is signatures
--   naming a zone which did not make them, and this is a signature
--   naming an algorithm which is not there.  All three end the same
--   way, and they get there by different routes.
--
--   See https://www.e-ontap.com/dns/samples.html
module BadAlgorithmSpec (spec) where

import DNS.SEC
import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "bad-algorithm") $
    describe "a signature naming an algorithm the zone has no key for" $ do
        -- There is no DNSKEY at example. with algorithm 8, so this
        -- signature cannot be checked against anything, and the parent
        -- said to check.  RFC 4035 Sec 5.3 has data which cannot be
        -- authenticated not handed on.
        it "refuses a name whose only signature is one it cannot check" $ \sc -> do
            a <- ask sc "fake.example." A
            answerRcode a `shouldBe` ServFail
            answerAuthentic a `shouldBe` False
            answerRRs a `shouldBe` []

        -- With CD the querier has said it will check for itself, so it
        -- gets the message as it came: the denial says the name is not
        -- there and the answer section says here it is.  The forged
        -- RRSIG is not among what comes back -- bowline keeps the
        -- signatures it could make sense of, and this was not one.
        it "hands that name over unchecked when the querier says CD" $ \sc -> do
            a <- askChecking sc "fake.example." A
            answerRcode a `shouldBe` NXDomain
            rdataOf A a `shouldBe` [rd_a "192.0.2.99"]

        -- RFC 4035 Sec 5.3.3: one signature which verifies is enough.
        -- www.example. arrives with the forged RRSIG next to its own,
        -- and its own is good, so the answer stands and is marked
        -- validated.
        it "still takes a name whose good signature came with a bad one" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- And what it hands back is the signature it used, not the one
        -- it could not: nothing downstream is asked to make sense of
        -- algorithm 8 either.
        it "passes on only the signature which verified" $ \sc -> do
            a <- ask sc "www.example." A
            [rrsig_pubalg sig | rr <- answerRRs a, Just sig <- [fromRData $ rdata rr]]
                `shouldBe` [ED25519]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

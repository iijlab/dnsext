{-# LANGUAGE OverloadedStrings #-}

-- | A reverse lookup which has to follow a CNAME across a delegation.
--
--   The holder of 150.42.6.0\/24 has given the first sixty-four
--   addresses to somebody else, and a zone cut cannot be made in the
--   middle of a label -- there is no name which means "the first
--   sixty-four" to cut at.  RFC 2317 gets round it by inventing one:
--   the piece is called @0-63.6.42.150.in-addr.arpa@, that name is
--   delegated, and each address in the range is pointed at its name
--   inside the delegation with a CNAME.
--
--   So looking up 150.42.6.1 asks for @1.6.42.150.in-addr.arpa@, is
--   sent by a CNAME to @1.0-63.6.42.150.in-addr.arpa@, and has to
--   follow a delegation to find it -- two cuts below the root, which is
--   why this needs both of the scenario's servers and the arrangement
--   "DeepDelegationSpec" describes.
--
--   See https://www.e-ontap.com/dns/samples.html and
--   https://meetings.ripe.net/ripe-50/presentations/ripe50-dns-in-bailiwick.pdf
module ClasslessReverseSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "classless-reverse") $
    describe "a reverse name split below the last label" $ do
        -- The whole point: the name resolves, through the indirection.
        it "resolves through the CNAME into the delegated piece" $ \sc -> do
            a <- ask sc "1.6.42.150.in-addr.arpa." PTR
            answerRcode a `shouldBe` NoErr
            rdataOf CNAME a `shouldBe` [rd_cname "1.0-63.6.42.150.in-addr.arpa."]
            rdataOf PTR a `shouldBe` [rd_ptr "one.example."]

        -- And it really did go to the other server for the piece, which
        -- is the delegation being followed rather than stepped over.
        it "follows the delegation to the server holding the piece" $ \sc -> do
            _ <- ask sc "1.6.42.150.in-addr.arpa." PTR
            root <- asked sc TheRoot
            prim <- asked sc ThePrimary
            prim `shouldSatisfy` elem ("1.6.42.150.in-addr.arpa.", PTR)
            root `shouldSatisfy` elem ("1.0-63.6.42.150.in-addr.arpa.", PTR)

        -- An address the holder of the \/24 kept is answered where it
        -- stands, with no CNAME and nothing to follow.
        it "answers an address which was not given away directly" $ \sc -> do
            a <- ask sc "200.6.42.150.in-addr.arpa." PTR
            answerRcode a `shouldBe` NoErr
            rdataOf CNAME a `shouldBe` []
            rdataOf PTR a `shouldBe` [rd_ptr "kept.example."]
            answerAuthentic a `shouldBe` True

        -- The piece is delegated without a DS, so what comes back for
        -- an address inside it is an unsigned answer reached through a
        -- signed CNAME.  Half of it validates and the answer does not.
        it "does not claim to have validated the piece, which is insecure" $ \sc -> do
            a <- ask sc "1.6.42.150.in-addr.arpa." PTR
            answerAuthentic a `shouldBe` False

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

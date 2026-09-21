{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does when a server denies a name which is there.
--
--   @a.b.c.example.@ is the only name in the zone with any records.
--   @c.example.@ and @b.c.example.@ exist because it does -- they are
--   empty non-terminals, and NOERROR with nothing in it is what each of
--   them is owed.  This server denies them instead, which is what
--   gouv.fr did and left a TXT record about.
--
--   bowline walks down a name one label at a time (see "QnameMinSpec"),
--   so it asks for both of those before it asks for the name anybody
--   wanted, and is told twice that there is nothing there.  Two rules
--   pull opposite ways at that point:
--
--   * RFC 9156 Sec 2.3 -- a resolver minimising the query name must not
--     take NXDOMAIN for an intermediate name as final, precisely
--     because servers like this one exist.
--
--   * RFC 8020 -- NXDOMAIN for a name means there is nothing below it
--     either.
--
--   The zone is unsigned on purpose, so that a resolver turning an
--   answer down has only the one reason to.
--
--   See https://www.e-ontap.com/dns/samples.html
module BrokenEntSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "broken-ent") $
    describe "a server which denies an empty non-terminal" $ do
        -- It keeps going, and gets there.
        it "reaches the name below the denials" $ \sc -> do
            a <- ask sc "a.b.c.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- It asks for all three, in order, and is denied the first two.
        it "was denied twice on the way down" $ \sc -> do
            _ <- ask sc "a.b.c.example." A
            qs <- asked sc ThePrimary
            [n | (n, t) <- qs, t == A]
                `shouldBe` ["c.example.", "b.c.example.", "a.b.c.example."]

        -- Asked for the denied name itself, bowline passes the server's
        -- word on.  It is the server's zone and its answer to give; the
        -- question is only what else may be concluded from it.
        it "hands the denial on when that is the question" $ \sc -> do
            a <- ask sc "c.example." A
            answerRcode a `shouldBe` NXDomain

        -- Nothing is concluded from it.  Having just been told that
        -- c.example. does not exist, bowline still answers for a name
        -- three labels below it -- which is where RFC 8020 is given up
        -- in favour of RFC 9156, and the only way this zone works.
        it "does not let the denial stand for what is below it" $ \sc -> do
            _ <- ask sc "c.example." A
            a <- ask sc "a.b.c.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

        -- And a denial which is true is still a denial: bowline has not
        -- simply stopped believing them.
        it "still denies a name which really is not there" $ \sc -> do
            a <- ask sc "nope.example." A
            answerRcode a `shouldBe` NXDomain

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

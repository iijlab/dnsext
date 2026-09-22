{-# LANGUAGE OverloadedStrings #-}

-- | What bowline makes of a zone which signs its data under a
--   delegation that carries no DS.
--
--   insecure.mufj.jp did that in 2020, and resolvers did not agree
--   about it: unbound, OpenDNS, AdGuard, Quad101, IIJ and others
--   answered SERVFAIL, while BIND, Knot, PowerDNS, 8.8.8.8 and 1.1.1.1
--   answered NOERROR.  The delegation has no DS, so the child is
--   insecure and its signatures are nobody's business; a validator
--   which looks at them anyway cannot verify them and calls the answer
--   bogus.  Which of the two a resolver does is a choice, and this is
--   where bowline's is written down.
--
--   See https://www.e-ontap.com/dns/amagasaki2020-neg/
module JpSharpSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

-- | One world for the three questions: standing it up means generating
--   keys and signing two zones, which is not worth doing three times.
spec :: Spec
spec = aroundAll (withScenario "jp.sharp") $
    describe "a zone which signs under a delegation with no DS" $ do
        -- The world is worth nothing if the part of it which is meant
        -- to validate does not.
        it "validates the parent, which is signed and vouched for" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True

        -- No DS, so nothing below the delegation is validated, whatever
        -- signatures it carries.
        it "answers below the delegation without saying it validated" $ \sc -> do
            a <- ask sc "host.insecure.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` False
            map rdata (answerRRs a) `shouldBe` [rd_a "192.0.2.9"]

        -- The signatures the child sends name a zone which did not
        -- sign them, so no key bowline can reach will verify them.
        -- Under a delegation with no DS they are not its business, and
        -- this is where it says so: the answer comes back, unvalidated,
        -- rather than as SERVFAIL.
        it "does not call it bogus when the signatures name another zone" $ \sc -> do
            a <- ask sc "insecure.example." NS
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` False

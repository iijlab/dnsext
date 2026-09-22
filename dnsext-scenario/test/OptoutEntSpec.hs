{-# LANGUAGE OverloadedStrings #-}

-- | Denying a name under an empty non-terminal in a zone signed with
--   Opt-Out.
--
--   @gov.example.@ has no records of its own.  It exists because
--   @sub.gov.example.@ is delegated below it, and that delegation
--   carries no DS -- so it is an empty non-terminal which exists only
--   because of an insecure delegation.
--
--   RFC 5155 Sec 7.1 lets a zone using Opt-Out leave such a name out of
--   the chain.  A zone which does has nothing to match the closest
--   encloser of a name below it with, so an NXDOMAIN there cannot be
--   proved, and resolvers disagree about what to do then.
--   hoge.gov.mufj.jp is where 「これも SERVFAIL する実装としない実装が
--   ある」, and the conclusion quoted there is "DO NOT USE THE NSEC3
--   OPT-OUT BIT".
--
--   clove keeps it: Opt-Out leaves out the delegations which carry no
--   DS and nothing else.  So the encloser is there to be matched, the
--   denial can be proved, and this scenario is where that is written
--   down -- a zone signed the way the warning is about, answering the
--   way it should.
--
--   See https://www.e-ontap.com/dns/samples.html
module OptoutEntSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "optout-ent") $
    describe "a name denied under an empty non-terminal, Opt-Out in use" $ do
        -- The one the warning is about.
        it "proves the denial below the empty non-terminal" $ \sc -> do
            a <- ask sc "hoge.gov.example." A
            answerRcode a `shouldBe` NXDomain
            answerAuthentic a `shouldBe` True

        -- And the empty non-terminal itself, which is there and owns
        -- nothing: NOERROR with nothing in it, proved.  That it can be
        -- answered at all is the chain carrying it.
        it "proves that the empty non-terminal has nothing" $ \sc -> do
            a <- ask sc "gov.example." A
            answerRcode a `shouldBe` NoErr
            answerRRs a `shouldBe` []
            answerAuthentic a `shouldBe` True

        -- The rest of the zone is unremarkable, which is worth saying:
        -- Opt-Out is about what is left out of the chain and not about
        -- what the zone answers.
        it "is otherwise an ordinary signed zone" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            answerAuthentic a `shouldBe` True
            [rdata rr | rr <- answerRRs a, rrtype rr == A] `shouldBe` [rd_a "192.0.2.1"]

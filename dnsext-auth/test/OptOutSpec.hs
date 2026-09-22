{-# LANGUAGE OverloadedStrings #-}

-- | What a referral to an unsigned subzone carries.
--
--   A zone which leaves its insecure delegations out of the NSEC3 chain
--   is using Opt-Out (RFC 5155 Sec 6), and proves one of them by naming
--   the NSEC3 which matches the closest encloser and the one which
--   covers the next closer name.  Where nothing of the zone hashes
--   between the two, there is only one such NSEC3 and it has both jobs.
module OptOutSpec where

import Data.Either (fromRight)
import Data.List (nub, sort)
import Test.Hspec

import DNS.Auth.Algorithm
import DNS.Auth.DB
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import Common

spec :: Spec
spec = describe "a referral to an unsigned subzone" $ do
    runIO $ runInitIO addResourceDataForDNSSEC
    let zone = "."
        build optOut = do
            rrs <- loadZoneFile zone "test/optout.zone"
            (_pub, _pri, dnskey, _ds, doSign) <-
                prepareDNSSEC $ defaultKeyConfig{keyConfZone = zone}
            -- No salt and no extra iterations, which is what BCP 236
            -- asks for and what the hashes in the zone file were worked
            -- out with.
            let salt = fromRight (error "fromBase16") $ Opaque.fromBase16 ""
                n3p = RD_NSEC3PARAM Hash_SHA1 0 0 salt
                n3c = (nsec3Config n3p){nsec3OptOut = optOut}
            makeDBforPrimary Checked zone (Just n3c) doSign doSign (rrs ++ [dnskey])
    db <- runIO $ build True

    -- Without this a resolver has no way to tell a name left out on
    -- purpose from one forged away, and dnsext's own validator turns
    -- the proof down.
    it "says Opt-Out on the NSEC3s it proves the delegation with" $ do
        let n3s = proofFor db "www.parentlong."
        n3s `shouldSatisfy` not . null
        n3s `shouldSatisfy` all (\n3 -> OptOut `elem` nsec3_flags n3)

    -- The interesting one: both jobs fall to the same record.
    it "sends the one NSEC3 once when it has both jobs to do" $ do
        let auth = authorityFor db "www.parentbrief."
        auth `shouldSatisfy` (not . null)
        auth `shouldBe` nub auth

    -- And the other, where they do not, still carries both.
    it "sends both where the two jobs fall to different records" $ do
        let auth = authorityFor db "www.parentlong."
        auth `shouldBe` nub auth
        length (proofFor db "www.parentlong.") `shouldBe` 2

    describe "and the same zone signed without Opt-Out" $ do
        plain <- runIO $ build False
        -- Every name is in the chain now, the insecure delegations
        -- among them, so each of them can be denied a DS outright
        -- rather than pointed at a gap.
        it "proves the delegation with the NSEC3 which matches it" $ do
            let n3s = proofFor plain "www.parentbrief."
            length n3s `shouldBe` 1
            n3s `shouldSatisfy` all (\n3 -> NS `elem` nsec3_types n3)
            n3s `shouldSatisfy` all (\n3 -> not $ DS `elem` nsec3_types n3)

        -- And says nothing about Opt-Out, because it is not using it.
        it "does not claim Opt-Out" $ do
            proofFor plain "www.parentbrief."
                `shouldSatisfy` all (\n3 -> not $ OptOut `elem` nsec3_flags n3)

        -- The one which used to need a gap needs none either.
        it "proves the other one the same way" $ do
            length (proofFor plain "www.parentlong.") `shouldBe` 1

        -- RFC 5155 Sec 7.1: the bitmap says what is at the name.  A
        -- delegation owns its NS, which the parent does not sign, so
        -- there is no RRSIG at it to name -- and with no DS either, NS
        -- is the whole of it.  This one is only reachable at all since
        -- a chain without Opt-Out started carrying such delegations.
        it "says NS and nothing else at a delegation it does not sign" $ do
            let n3s = proofFor plain "www.parentbrief."
            map (sort . nsec3_types) n3s `shouldBe` [[NS]]

authorityFor :: DB -> Domain -> [ResourceRecord]
authorityFor db dom = authority $ getAnswer db dnssecQuery{question = Question dom A IN}

proofFor :: DB -> Domain -> [RD_NSEC3]
proofFor db dom =
    [n3 | rr <- authorityFor db dom, rrtype rr == NSEC3, Just n3 <- [fromRData $ rdata rr]]

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
    db <- runIO $ do
        rrs <- loadZoneFile zone "test/optout.zone"
        (_pub, _pri, dnskey, _ds, doSign) <-
            prepareDNSSEC $ defaultKeyConfig{keyConfZone = zone}
        -- No salt and no extra iterations, which is what BCP 236 asks
        -- for and what the hashes in the zone file were worked out with.
        let salt = fromRight (error "fromBase16") $ Opaque.fromBase16 ""
            n3p = RD_NSEC3PARAM Hash_SHA1 0 0 salt
        makeDBforPrimary zone (Just n3p) doSign doSign (rrs ++ [dnskey])

    -- Without this a resolver has no way to tell a name left out on
    -- purpose from one forged away, and dnsext's own validator turns
    -- the proof down.
    it "says Opt-Out on the NSEC3s it proves the delegation with" $ do
        let n3s = proofFor db "www.parentlong."
        n3s `shouldSatisfy` not . null
        n3s `shouldSatisfy` all (\n3 -> OptOut `elem` nsec3_flags n3)


authorityFor :: DB -> Domain -> [ResourceRecord]
authorityFor db dom = authority $ getAnswer db dnssecQuery{question = Question dom A IN}

proofFor :: DB -> Domain -> [RD_NSEC3]
proofFor db dom =
    [n3 | rr <- authorityFor db dom, rrtype rr == NSEC3, Just n3 <- [fromRData $ rdata rr]]

{-# LANGUAGE OverloadedStrings #-}

-- | What a secondary has to hand on.
--
--   A zone reaches a secondary by transfer and may leave it the same
--   way -- a hidden primary feeding a few servers which feed the rest
--   is an ordinary arrangement, and clove will do it, @allow-transfer@
--   being a setting a secondary zone takes like any other.  Whatever
--   came in has to be able to go out again: a server further down the
--   line has nothing else to go on, and a zone which loses its
--   signatures on the way is a zone which is bogus to every resolver
--   which checks them, without anybody having said so.
module SecondarySpec where

import Data.List (sort)
import Test.Hspec

import DNS.Auth.DB
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types

spec :: Spec
spec = describe "a zone which came in by transfer" $ do
    runIO $ runInitIO addResourceDataForDNSSEC
    let zone = "example."
    (primary, secondary) <- runIO $ do
        rrs <- loadZoneFile zone "test/example.zone"
        (_pub, _pri, dnskey, _ds, doSign) <-
            prepareDNSSEC $ defaultKeyConfig{keyConfZone = zone}
        p <- makeDBforPrimary zone Nothing doSign doSign (rrs ++ [dnskey])
        s <- makeDBforSecondary zone $ dbAll p
        pure (p, s)

    it "goes out again as it came in" $
        shape (dbAll secondary) `shouldBe` shape (dbAll primary)

    -- Said separately because it is the part which goes quiet rather
    -- than wrong: a server fed from here would serve the zone unsigned
    -- under a DS its parent still has.
    it "goes out again with its signatures" $ do
        let sigs rrs = length [() | r <- rrs, rrtype r == RRSIG]
        sigs (dbAll primary) `shouldSatisfy` (> 0)
        sigs (dbAll secondary) `shouldBe` sigs (dbAll primary)

    it "goes out again with the records which deny a name" $ do
        let denials rrs = length [() | r <- rrs, rrtype r `elem` [NSEC, NSEC3]]
        denials (dbAll primary) `shouldSatisfy` (> 0)
        denials (dbAll secondary) `shouldBe` denials (dbAll primary)

-- | What is in a zone, without regard to the order it is in: RFC 5936
--   Sec 2.2 asks only that the SOA come first and last.
shape :: [ResourceRecord] -> [(Domain, TYPE)]
shape rrs = sort [(rrname r, rrtype r) | r <- rrs]

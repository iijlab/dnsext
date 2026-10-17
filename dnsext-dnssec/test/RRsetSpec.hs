{-# LANGUAGE OverloadedStrings #-}

module RRsetSpec (spec) where

import DNS.SEC.Verify
import DNS.Types
import Data.IP (IPv4)
import Test.Hspec

spec :: Spec
spec = describe "the TTL of a canonical RRset" $ do
    -- RFC 2181 Sec 5.2: a client takes the TTLs of an RRset as the
    -- lowest of them.  The TTL of the record which sorted first was
    -- taken instead, and the sort is on the RDATA, not on the TTL.
    it "is the lowest of the TTLs it was given" $
        ttlOf [rr 300 "192.0.2.1", rr 60 "192.0.2.2"] `shouldBe` Just 60

    it "is the lowest however the records are ordered" $
        ttlOf [rr 60 "192.0.2.2", rr 300 "192.0.2.1"] `shouldBe` Just 60

    it "is that TTL where they all agree" $
        ttlOf [rr 300 "192.0.2.1", rr 300 "192.0.2.2"] `shouldBe` Just 300

    it "is the TTL of the one record of a single record RRset" $
        ttlOf [rr 300 "192.0.2.1"] `shouldBe` Just 300

    it "is nothing at all for what is not an RRset" $ do
        ttlOf [] `shouldBe` Nothing
        ttlOf [rr 300 "192.0.2.1", ResourceRecord "other.example." A IN 60 $ rd_a "192.0.2.2"]
            `shouldBe` Nothing

ttlOf :: [ResourceRecord] -> Maybe TTL
ttlOf rrs = canonicalRRset rrs (const Nothing) (\_ _ _ ttl _ -> Just ttl)

rr :: TTL -> IPv4 -> ResourceRecord
rr ttl ip = ResourceRecord "www.example." A IN ttl $ rd_a ip

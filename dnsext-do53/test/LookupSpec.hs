{-# LANGUAGE OverloadedStrings #-}

module LookupSpec where

import DNS.Do53.Client as DNS
import Test.Hspec

import FakeServer (withAnswering)

-- | The lookups used to go to whatever /etc/resolv.conf named, and to
--   ask about mew.org, google.com and ipv4.tlund.se -- so the suite
--   needed the internet, and needed those names to keep the records it
--   expects of them.  They go to the fake server now, which has the
--   same names in it.
withFake :: (DNS.LookupEnv -> IO a) -> IO a
withFake body = withAnswering $ \port ->
    withLookupConf defaultLookupConf{lconfSeeds = SeedsAddrPort "127.0.0.1" port} body

spec :: Spec
spec = describe "lookup" $ do
    it "lookupA" $ withFake $ \resolver -> do
        addrs <- DNS.lookupA resolver "mew.org"
        -- mew.org has one or more IPv4 addresses
        fmap null addrs `shouldBe` Right False

    it "lookupAAAA" $ withFake $ \resolver -> do
        -- google.com has one or more IPv6 addresses
        addrs <- DNS.lookupAAAA resolver "google.com"
        fmap null addrs `shouldBe` Right False

    it "lookupAAAA with empty result" $ withFake $ \resolver -> do
        addrs <- DNS.lookupAAAA resolver "ipv4.tlund.se"
        fmap null addrs `shouldBe` Right True

    it "lookupMX" $ withFake $ \resolver -> do
        addrs <- DNS.lookupMX resolver "mew.org"
        -- mew.org has one or more MX records.
        fmap null addrs `shouldBe` Right False

    it "lookupTXT" $ withFake $ \resolver -> do
        addrs <- DNS.lookupTXT resolver "mew.org"
        -- mew.org has one or more TXT records.
        fmap null addrs `shouldBe` Right False

    -- This asked for the TXT records and called it a SOA test.
    it "lookupSOA" $ withFake $ \resolver -> do
        addrs <- DNS.lookupSOA resolver "mew.org"
        -- mew.org has a SOA record.
        fmap null addrs `shouldBe` Right False

    it "lookupNS" $ withFake $ \resolver -> do
        addrs <- DNS.lookupNS resolver "mew.org"
        -- mew.org has one or more NS records.
        fmap null addrs `shouldBe` Right False

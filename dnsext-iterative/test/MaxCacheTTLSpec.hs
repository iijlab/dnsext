{-# LANGUAGE OverloadedStrings #-}

module MaxCacheTTLSpec (spec) where

import DNS.Iterative.Internal
import qualified DNS.RRCache as Cache
import DNS.Types
import Test.Hspec

spec :: Spec
spec = describe "how long a positive answer is kept" $ do
    -- The negative side has had cache-max-negative-ttl and
    -- cache-failure-rcode-ttl all along; this side had nothing, so
    -- whatever a peer said stuck for as long as it said.
    it "is no longer than the ceiling, in the cache" $
        cachedTTL (10 * 86400) `shouldReturn` Just 86400

    it "is no longer than the ceiling, in the answer" $
        answeredTTL 86400 (10 * 86400) `shouldBe` 86400

    it "is left alone below the ceiling" $
        answeredTTL 86400 3600 `shouldBe` 3600

    it "has a ceiling of a day until it is configured otherwise" $ do
        env <- newEmptyEnv
        maxCacheTTL_ env `shouldBe` 86400

name :: Domain
name = "www.example."

-- | The TTL of the RRset an answer is built from.  With no DNSKEY to
--   verify against there is nothing to lower it, so what comes out is
--   what came in, up to the ceiling.
answeredTTL :: TTL -> TTL -> TTL
answeredTTL lim ttl = withVerifiedRRset NoCheckDisabled 0 lim [] name (rrset ttl) [] [] rrsTTL

-- | The TTL the cache is left holding for a section which arrives
--   without signatures, which is the way glue arrives.
cachedTTL :: TTL -> IO (Maybe TTL)
cachedTTL ttl = do
    env0 <- newEmptyEnv
    (getCache, insert) <- newTestCache (currentSeconds_ env0) $ 2 * 1024
    let env = env0{insert_ = insert, getCache_ = getCache}
    _ <- runDNSQuery (cacheSection [rr ttl] Cache.RankAnswer) env noopWorkerStat qp
    now <- currentSeconds_ env
    cache <- getCache_ env
    pure $ case Cache.lookup now name A IN cache of
        Just (x : _, _) -> Just $ rrttl x
        _ -> Nothing
  where
    qp = queryParamIN name A mempty

rr :: TTL -> ResourceRecord
rr ttl = ResourceRecord name A IN ttl $ rd_a "192.0.2.1"

rrset :: TTL -> RRset
rrset ttl = RRset name A IN ttl [rd_a "192.0.2.1"] notValidNoSig

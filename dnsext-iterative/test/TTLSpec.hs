{-# LANGUAGE OverloadedStrings #-}

module TTLSpec (spec) where

import DNS.Iterative.Internal
import qualified DNS.RRCache as Cache
import DNS.Types
import Test.Hspec

spec :: Spec
spec = describe "the TTL an RRset arrives with" $ do
    -- RFC 2181 Sec 8: the TTL is a 31-bit unsigned value, and one which
    -- arrives with the top bit set is to be taken as zero.  Nothing
    -- looked at it, and `now <+ ttl` put such an entry 136 years out.
    it "is zero when the top bit is set" $
        answeredTTL 0xffffffff `shouldBe` 0

    it "is taken as it stands below that" $
        answeredTTL 3600 `shouldBe` 3600

    it "keeps nothing in the cache when the top bit is set" $
        cachedTTL 0xffffffff `shouldReturn` Nothing

    it "is kept in the cache as it stands below that" $
        cachedTTL 3600 `shouldReturn` Just 3600

name :: Domain
name = "www.example."

-- | The TTL of the RRset an answer is built from.  With no DNSKEY to
--   verify against there is nothing to lower it, so what comes out is
--   what came in.
answeredTTL :: TTL -> TTL
answeredTTL ttl = withVerifiedRRset NoCheckDisabled 0 [] name (rrset ttl) [] [] rrsTTL

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

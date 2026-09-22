{-# LANGUAGE OverloadedStrings #-}

-- | Whether the cache keeps a record for as long as it was told to, and
--   no longer.
--
--   The zone has two names with the same data and very different TTLs.
--   How long each may be kept is the only thing which tells them apart,
--   so how often the server is asked for each says what the cache made
--   of the number it was given.  This is the one thing here which cannot
--   be seen in a single answer, and it is read off the server's own log.
--
--   A cache which keeps a record past its TTL is serving something its
--   owner has stopped standing behind, and the owner has no way to know
--   or to stop it -- which is what a short TTL before a planned move is
--   for.  a.t.e-ontap.com asks it of whatever resolver a client happens
--   to be behind: 「ISPのDNSキャッシュサーバはTTLを越えてキャッシュを
--   保持するか?」
--
--   Nothing is signed: what is being timed is the cache, and a signed
--   zone would have the life of its signatures to time as well.
--
--   See https://www.e-ontap.com/dns/samples.html
module CacheTtlSpec (spec) where

import Control.Concurrent (threadDelay)
import DNS.Types
import Test.Hspec

import Harness

-- | Longer than the brief record's two seconds, by enough that a busy
--   machine does not decide the matter.
pastTheBriefOne :: Int
pastTheBriefOne = 3000000

spec :: Spec
spec = aroundAll (withScenario "cache-ttl") $
    describe "how long a record is kept" $ do
        it "keeps each one for as long as its TTL says, and no longer" $ \sc -> do
            atFirst <- ttlOf <$> ask sc "long.example." A
            _ <- ask sc "brief.example." A
            -- Both are in the cache now, and asking again does not
            -- reach the server.
            _ <- ask sc "long.example." A
            _ <- ask sc "brief.example." A
            threadDelay pastTheBriefOne
            later <- ttlOf <$> ask sc "long.example." A
            _ <- ask sc "brief.example." A
            qs <- asked sc ThePrimary
            let times n = length [() | (m, A) <- qs, m == n]
            -- The one with an hour on it was fetched once and answered
            -- from the cache every time after.
            times "long.example." `shouldBe` 1
            -- The one with two seconds on it was fetched again once its
            -- time was up, and not before.
            times "brief.example." `shouldBe` 2
            -- And what the client is told is the time left, not the
            -- time the zone first said.
            later `shouldSatisfy` \t -> maybe False (< 3600) t
            later `shouldSatisfy` \t -> t < atFirst

        it "never hands out more time than the zone gave" $ \sc -> do
            long <- ttlOf <$> ask sc "long.example." A
            brief <- ttlOf <$> ask sc "brief.example." A
            long `shouldSatisfy` \t -> maybe False (<= 3600) t
            brief `shouldSatisfy` \t -> maybe False (<= 2) t

-- | The TTL on the record asked about, where there is one.
ttlOf :: Answer -> Maybe TTL
ttlOf a = case [rrttl rr | rr <- answerRRs a, rrtype rr == A] of
    t : _ -> Just t
    [] -> Nothing

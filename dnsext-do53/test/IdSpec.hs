module IdSpec where

import Control.Monad (replicateM)
import qualified Data.Set as Set
import Test.Hspec

import DNS.Do53.Internal

-- | Twenty thousand identifiers drawn from the sixteen bits there are.
--
--   Picking evenly, about 17,240 of them come out distinct -- the rest
--   are repeats, which is what drawing with replacement means.  A
--   generator which is stuck gives one distinct value, and one which
--   walks through the range in order gives twenty thousand; both are
--   outside these bounds and nothing sound is anywhere near them.  The
--   spread of the real figure is a few tens, so the bounds are many
--   times that away and this does not fail on a bad day.
--
--   It says nothing about whether the numbers can be predicted, which
--   is the point of using a stream cipher for them and is not a thing
--   a test can show.
draws :: Int
draws = 20000

spec :: Spec
spec = describe "the identifier on a query" $ do
    it "is spread across the range, with the repeats that implies" $ do
        ids <- replicateM draws singleGenId
        distinct ids `shouldSatisfy` \d -> 16000 <= d && d <= 18500

    it "is spread the same way when each capability has its own" $ do
        gen <- newConcurrentGenId
        ids <- replicateM draws gen
        distinct ids `shouldSatisfy` \d -> 16000 <= d && d <= 18500
  where
    distinct = Set.size . Set.fromList

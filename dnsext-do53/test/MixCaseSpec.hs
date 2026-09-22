{-# LANGUAGE OverloadedStrings #-}

module MixCaseSpec where

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import Test.Hspec

import DNS.Do53.Internal
import DNS.Types

-- | 'mixCase' spends one bit on each letter, and a bit which is not set
--   asks for upper case.  So a generator which is all noughts gives the
--   name in upper case throughout and one which is all ones gives it in
--   lower case, which is a way of watching it work without watching it
--   guess.
spec :: Spec
spec = describe "mixing the case of a name" $ do
    it "spends a nought on a letter to put it in upper case" $ do
        d <- mixCase "www.example." noughts
        originalWireLabels d `shouldBe` ["WWW", "EXAMPLE"]

    it "spends a one to put it in lower case" $ do
        d <- mixCase "WWW.EXAMPLE." ones
        originalWireLabels d `shouldBe` ["www", "example"]

    it "leaves alone what has no case to mix" $ do
        d <- mixCase "10.0-63.in-addr.arpa." noughts
        originalWireLabels d `shouldBe` ["10", "0-63", "IN-ADDR", "ARPA"]

    -- The whole point of folding case in Eq: a name is the same name
    -- however it is written, so nothing compares, orders or is looked up
    -- differently for having been mixed.
    it "gives back the same name" $ do
        mix <- newConcurrentMixCase
        ds <- replicateM 50 $ mix "www.example."
        all (== "www.example.") ds `shouldBe` True

    -- And it is not the same bytes: over fifty draws of a name with ten
    -- letters in it, every one coming out the same way is 2^-490.
    it "does not write it the same way twice running" $ do
        mix <- newConcurrentMixCase
        ds <- replicateM 50 $ mix "www.example."
        length (unique $ map originalWireLabels ds) `shouldSatisfy` (> 1)

    it "tells apart what is written differently" $ do
        upper <- mixCase "www.example." noughts
        lower <- mixCase "www.example." ones
        sameCase upper upper `shouldBe` True
        sameCase upper lower `shouldBe` False
  where
    -- A generator which draws whatever is asked for and always the
    -- same, so that the mixing can be watched without watching it
    -- guess.
    noughts n = pure $ BS.replicate n 0x00
    ones n = pure $ BS.replicate n 0xFF
    unique = foldr (\x xs -> if x `elem` xs then xs else x : xs) []

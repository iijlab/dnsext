{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Test.Hspec

import DNS.Types

spec :: Spec
spec = do
    describe "leafDomain" $ do
        it "returns the left most label" $ do
            leafDomain "www.example.jp." `shouldBe` Just "www"
            leafDomain "example.jp." `shouldBe` Just "example"

        -- A domain of a single label used to be reported as having no
        -- leaf at all, because the guard looked at the upper bound of
        -- the label array instead of at the number of labels.
        it "returns the label of a single label domain" $ do
            leafDomain "jp." `shouldBe` Just "jp"
            leafDomain "example." `shouldBe` Just "example"

        it "recognizes a wildcard" $ do
            leafDomain "*.example.jp." `shouldBe` Just "*"
            leafDomain "*." `shouldBe` Just "*"

        -- The root used to fall through the guard and index an empty
        -- array, which threw.
        it "returns Nothing for the root" $
            leafDomain "." `shouldBe` Nothing

        it "agrees with the number of labels" $
            mapM_ agree ["www.example.jp.", "example.jp.", "jp.", "."]

    describe "unsafeLeafDomain" $ do
        it "returns the left most label" $ do
            unsafeLeafDomain "www.example.jp." `shouldBe` "www"
            unsafeLeafDomain "jp." `shouldBe` "jp"

        it "returns \".\" for the root" $
            unsafeLeafDomain "." `shouldBe` "."

agree :: Domain -> Expectation
agree d = case leafDomain d of
    Nothing -> labelsCount d `shouldBe` 0
    Just l -> do
        labelsCount d `shouldNotBe` 0
        Just l `shouldBe` (headOf . revLabels) d
  where
    headOf ls = case reverse ls of
        [] -> Nothing
        x : _ -> Just x

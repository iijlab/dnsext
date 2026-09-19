{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Control.Exception (evaluate)
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

    -- RFC 1035 Sec 2.3.4 gives a name 255 octets on the wire, which
    -- counts a length octet before each label and the root label at the
    -- end.  The check counted the labels and nothing else, so a name of
    -- four labels of 63 -- 252 octets of label and 257 on the wire --
    -- was let through.
    describe "the length of a name from its representation" $ do
        it "allows a name of 255 octets" $
            domainSize (labelsOf [63, 63, 63, 61]) `shouldBe` 255

        it "refuses a name of 256 octets" $
            evaluate (domainSize (labelsOf [63, 63, 63, 62])) `shouldThrow` illegal

        it "refuses four labels of 63, which is 257 octets" $
            evaluate (domainSize (labelsOf [63, 63, 63, 63])) `shouldThrow` illegal

        it "refuses a label of 64" $
            evaluate (domainSize (labelsOf [64])) `shouldThrow` illegal

    describe "unsafeLeafDomain" $ do
        it "returns the left most label" $ do
            unsafeLeafDomain "www.example.jp." `shouldBe` "www"
            unsafeLeafDomain "jp." `shouldBe` "jp"

        it "returns \".\" for the root" $
            unsafeLeafDomain "." `shouldBe` "."

-- | A name of labels of the given lengths.
labelsOf :: [Int] -> Domain
labelsOf ns = fromRepresentation $ concat [replicate n 'a' ++ "." | n <- ns]

illegal :: Selector DNSError
illegal = (== IllegalDomain)

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

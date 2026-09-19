{-# LANGUAGE OverloadedStrings #-}

module DomainSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Data.List (sort)
import qualified Data.Map as Map
import Test.Hspec

import DNS.Types
import DNS.Types.Decode (decode)
import DNS.Types.Encode (encode)
import DNS.Types.Internal (CanonicalFlag (..), putDomain, runBuilder)

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
    -- A name keeps the case it arrived in so that it can be given back
    -- (RFC 4343 Sec 3, RFC 5452 Sec 9.1), and that must not make it a
    -- different name from the same one in lower case.
    describe "the case of a name" $ do
        it "does not change what the name is equal to" $ do
            (mixed :: Domain) `shouldBe` lower
            lower `shouldBe` mixed

        it "does not change how names order" $ do
            compare mixed lower `shouldBe` EQ
            compare mixed ("HOST778.Z.JP" :: Domain) `shouldBe` compare lower ("host778.z.jp" :: Domain)
            sort [mixed, "A.Z.JP", "b.z.jp"] `shouldBe` sort [lower, "a.z.jp", "B.Z.JP"]

        it "does not change what a name finds in a map" $ do
            Map.lookup mixed (Map.singleton lower ()) `shouldBe` Just ()
            Map.lookup lower (Map.singleton mixed ()) `shouldBe` Just ()

        it "does not change the representation" $ do
            toRepresentation mixed `shouldBe` ("host777.z.jp." :: String)
            show mixed `shouldBe` show lower

        it "does not change what the labels are" $
            wireLabels mixed `shouldBe` wireLabels lower

        -- The point of keeping it at all.
        it "comes back on the wire as it was given" $
            originalWireLabels fromWire `shouldBe` ["HOST777", "Z", "JP"]

        it "is folded in the canonical form, which is what DNSSEC signs" $ do
            canonical fromWire `shouldBe` canonical lower
            original fromWire `shouldNotBe` original lower
            canonical fromWire `shouldBe` original lower

        it "survives a decode and an encode of a whole message" $ do
            BS.drop 12 (encode queryMessage) `shouldBe` BS.drop 12 queryWire

        -- fromWireLabels did not fold, so a name built that way was not
        -- equal to the same name from its representation.
        it "is folded when a name is built from labels" $ do
            fromLabels `shouldBe` "www.example.com."
            wireLabels fromLabels `shouldBe` ["www", "example", "com"]
            originalWireLabels fromLabels `shouldBe` ["WWW", "Example", "COM"]

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
mixed :: Domain
mixed = "HOST777.Z.JP"

-- | A name built from labels rather than from a representation.
fromLabels :: Domain
fromLabels = fromWireLabels (["WWW", "Example", "COM"] :: [ShortByteString])

lower :: Domain
lower = "host777.z.jp"

-- | The same name, read off the wire rather than from a string, which
--   is where the case is kept.
fromWire :: Domain
fromWire = qname $ question queryMessage

queryMessage :: DNSMessage
queryMessage = case decode queryWire of
    Right m -> m
    Left e -> error $ show e

queryWire :: BS.ByteString
queryWire =
    BS.pack [0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 7]
        <> "HOST777"
        <> BS.pack [1]
        <> "Z"
        <> BS.pack [2]
        <> "JP"
        <> BS.pack [0, 0, 1, 0, 1]

canonical :: Domain -> BS.ByteString
canonical d = runBuilder (domainSize d) $ putDomain Canonical d

original :: Domain -> BS.ByteString
original d = runBuilder (domainSize d) $ putDomain Original d

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

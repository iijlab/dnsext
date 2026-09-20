module TypeSpec (spec) where

import DNS.Types
import Text.Read (readMaybe)
import Test.Hspec

spec :: Spec
spec = describe "Read TYPE" $ do
    it "reads a mnemonic, in any case" $ do
        readMaybe "A" `shouldBe` Just A
        readMaybe "aaaa" `shouldBe` Just AAAA

    -- RFC 3597 Sec 5: the mnemonic for a type with no name of its own
    -- is "TYPE" and the decimal value.
    it "reads the RFC 3597 form" $ do
        readMaybe "TYPE65534" `shouldBe` Just (toTYPE 65534)
        readMaybe "TYPE0" `shouldBe` Just (toTYPE 0)

    -- The value was read with "read" into a Word16, and fromInteger
    -- takes what it is given: TYPE99999 was read as TYPE34463 and
    -- TYPE-1 as TYPE65535, each without a word.
    it "refuses a value no type number can hold" $ do
        readMaybe "TYPE99999" `shouldBe` (Nothing :: Maybe TYPE)
        readMaybe "TYPE65536" `shouldBe` (Nothing :: Maybe TYPE)
        readMaybe "TYPE-1" `shouldBe` (Nothing :: Maybe TYPE)

    -- "read" fails by throwing, which neither reads nor readMaybe can
    -- catch, so what should have been "this is not a type" was an
    -- ErrorCall out of pure code: it took dug down and it turned a
    -- zone file with a typo in it into a crash rather than a parse
    -- error.
    it "refuses what is not a number at all" $ do
        readMaybe "TYPEABC" `shouldBe` (Nothing :: Maybe TYPE)
        readMaybe "TYPE" `shouldBe` (Nothing :: Maybe TYPE)
        readMaybe "TYPE1X" `shouldBe` (Nothing :: Maybe TYPE)

    -- The Haskell lexer takes 0x10 for 16 and 0o7 for 7; RFC 3597 says
    -- decimal.
    it "refuses a number which is not written in decimal" $ do
        readMaybe "TYPE0X10" `shouldBe` (Nothing :: Maybe TYPE)
        readMaybe "TYPE0O7" `shouldBe` (Nothing :: Maybe TYPE)

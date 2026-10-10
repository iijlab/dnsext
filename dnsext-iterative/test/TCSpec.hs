{-# LANGUAGE OverloadedStrings #-}

module TCSpec (spec) where

import DNS.Iterative.Server
import DNS.Types
import qualified DNS.Types as DNS
import DNS.Types.Encode (encode)
import qualified Data.ByteString as BS
import Network.Socket (SockAddr (..))
import Test.Hspec

spec :: Spec
spec = describe "an answer which has to fit a UDP buffer" $ do
    -- RFC 2181 Sec 9 asks that TC not be set merely because the
    -- additional section did not fit, so the additional section goes
    -- first and the answer is still sent whole.
    it "is sent whole with room to spare" $ do
        let r = fitted (whole + 1) reply
        DNS.additional r `shouldBe` DNS.additional reply
        trunCation (flags r) `shouldBe` False

    -- The guard was `len r0 < lim` where its own comment said
    -- `len r0 <= lim`, so an answer which came to exactly the length
    -- offered was stripped of its additional section for no reason.
    it "is sent whole when it exactly fills the buffer" $ do
        let r = fitted whole reply
        DNS.additional r `shouldBe` DNS.additional reply
        trunCation (flags r) `shouldBe` False

    it "loses its additional section when it is one octet too long" $ do
        let r = fitted (whole - 1) reply
        DNS.additional r `shouldBe` []
        DNS.answer r `shouldBe` DNS.answer reply
        trunCation (flags r) `shouldBe` False

    it "is sent without its additional section when that exactly fits" $ do
        let r = fitted stripped reply
        DNS.additional r `shouldBe` []
        DNS.answer r `shouldBe` DNS.answer reply
        trunCation (flags r) `shouldBe` False

    it "is truncated when even that does not fit" $ do
        let r = fitted (stripped - 1) reply
        trunCation (flags r) `shouldBe` True
        DNS.answer r `shouldBe` []
        DNS.authority r `shouldBe` []
        DNS.additional r `shouldBe` []

whole :: Int
whole = BS.length $ encode reply

stripped :: Int
stripped = BS.length $ encode reply{DNS.additional = []}

-- | What the server would send over UDP, given that much room.
fitted :: Int -> DNSMessage -> DNSMessage
fitted lim msg = handleTC peer (\r _ -> r) (fromIntegral lim) msg
  where
    peer = PeerInfoUDP (SockAddrInet 53 0) []

reply :: DNSMessage
reply =
    defaultResponse
        { question = Question "www.example." NS IN
        , DNS.answer = [rr NS $ rd_ns "ns1.example."]
        , DNS.additional = [rr' "ns1.example." A $ rd_a "192.0.2.1"]
        }
  where
    rr typ rd = rr' "www.example." typ rd
    rr' name typ rd = ResourceRecord name typ IN 3600 rd

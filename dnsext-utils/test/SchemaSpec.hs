{-# LANGUAGE OverloadedStrings #-}

module SchemaSpec where

import DNS.Types
import Data.ByteString ()
import Test.Hspec

import DNS.TAP.Schema

gMSG :: Message
gMSG =
    Message
        { messageType = CLIENT_QUERY
        , messageSocketFamily = Just IPv4
        , messageSocketProtocol = Just UDP
        , messageQueryAddress = Just "127.0.0.1"
        , messageResponseAddress = Just "127.0.0.1"
        , messageQueryPort = Just 5000
        , messageResponsePort = Just 53
        , messageQueryTimeSec = Just 1693364212
        , messageQueryTimeNsec = Just 267921000
        , messageQueryMessage = Just $ DnsMsg defaultQuery
        , messageQueryZone = Nothing
        , messageResponseTimeSec = Nothing
        , messageResponseTimeNsec = Nothing
        , messageResponseMessage = Nothing
        , messagePolicy = Nothing
        , messageHttpProtocol = Just HTTP_NONE
        }

spec :: Spec
spec = do
    describe "encode & decode" $ do
        it "can encode then decode" $ do
            roundTripMessage gMSG

roundTripMessage :: Message -> Expectation
roundTripMessage msg = decodeMessage (encodeMessage msg) `shouldBe` msg

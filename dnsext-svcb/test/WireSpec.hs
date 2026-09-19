{-# LANGUAGE OverloadedStrings #-}

module WireSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Test.Hspec

import DNS.SVCB
import DNS.Types
import DNS.Types.Decode (decode)

-- | An answer holding one SVCB record with the given SvcParams after
--   it, in the order they are given.
svcbWith :: [ByteString] -> ByteString
svcbWith params =
    BS.pack [0, 1, 0x80, 0, 0, 1, 0, 1, 0, 0, 0, 0]
        <> qname
        <> BS.pack [0, 64, 0, 1]
        <> BS.pack [0xc0, 0x0c, 0, 64, 0, 1, 0, 0, 0, 60]
        <> BS.pack [0, fromIntegral (BS.length rdata_)]
        <> rdata_
  where
    qname = BS.pack [3] <> "www" <> BS.pack [7] <> "example" <> BS.pack [0]
    rdata_ = BS.pack [0, 1] <> BS.pack [0 {- the root as TargetName -}] <> BS.concat params

param :: Int -> ByteString -> ByteString
param key v =
    BS.pack [fromIntegral (key `div` 256), fromIntegral (key `mod` 256)]
        <> BS.pack [fromIntegral (BS.length v `div` 256), fromIntegral (BS.length v `mod` 256)]
        <> v

alpn :: ByteString -> ByteString
alpn p = param 1 $ BS.pack [fromIntegral (BS.length p)] <> p

port :: ByteString
port = param 3 $ BS.pack [1, 187]

spec :: Spec
spec = describe "SvcParams off the wire" $ do
    runIO $ runInitIO addResourceDataForSVCB

    -- RFC 9460 Sec 2.2: "SvcParamKeys SHALL appear in increasing
    -- numeric order".  A record where they do not is malformed, and
    -- taking it anyway means taking what other implementations refuse
    -- and writing it back out in an order it did not arrive in.
    it "are taken in increasing order of key" $
        decode (svcbWith [alpn "h2", port]) `shouldSatisfy` isRight

    it "are refused out of order" $
        decode (svcbWith [port, alpn "h2"]) `shouldSatisfy` isLeft

    it "are refused when a key is repeated" $
        decode (svcbWith [alpn "h2", alpn "h3"]) `shouldSatisfy` isLeft

    it "are fine when there are none at all" $
        decode (svcbWith []) `shouldSatisfy` isRight

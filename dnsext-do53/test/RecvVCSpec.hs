{-# LANGUAGE OverloadedStrings #-}

-- | Reading one message off a virtual circuit.
module RecvVCSpec where

import Control.Exception (try)
import qualified Data.ByteString as BS
import Data.IORef
import Test.Hspec

import DNS.Do53.Internal
import DNS.Types (DNSError (..))

spec :: Spec
spec = describe "reading a message off a virtual circuit" $ do
    it "takes a message which arrived whole" $ do
        rcv <- chunks [len 4, "abcd"]
        recvVC 65535 rcv `shouldReturn` "abcd"

    it "takes a message which arrived in pieces" $ do
        rcv <- chunks [len 4, "ab", "cd"]
        recvVC 65535 rcv `shouldReturn` "abcd"

    -- The length said ten octets and four arrived, so this is not a
    -- message: it is the beginning of one, and the rest of it is never
    -- coming.  Handing it on as if it were whole only moves the failure
    -- to whoever tries to decode it.
    it "refuses a message which stopped short" $ do
        rcv <- chunks [len 10, "abcd"]
        thrown (recvVC 65535 rcv) `shouldReturn` True

    it "refuses a message which never started" $ do
        rcv <- chunks [len 10]
        thrown (recvVC 65535 rcv) `shouldReturn` True

    -- A length prefix which did not arrive either.  decodeVCLength
    -- calls a prefix it could not read zero octets long, so this must
    -- not come back as a message of no octets.
    it "refuses a circuit which said nothing at all" $ do
        rcv <- chunks []
        thrown (recvVC 65535 rcv) `shouldReturn` True

    it "refuses a message longer than the limit" $ do
        rcv <- chunks [len 4096, BS.replicate 4096 65]
        thrown (recvVC 1024 rcv) `shouldReturn` True

-- | A two-octet length prefix.
len :: Int -> BS.ByteString
len n = BS.pack [fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]

-- | A reader which hands over these in turn and then end-of-file.
chunks :: [BS.ByteString] -> IO (IO BS.ByteString)
chunks bss = do
    ref <- newIORef bss
    pure $ atomicModifyIORef' ref next
  where
    next [] = ([], BS.empty)
    next (b : bs) = (bs, b)

-- | Whether the action threw a 'DNSError'.
thrown :: IO a -> IO Bool
thrown act = either (\e -> const True (e :: DNSError)) (const False) <$> try act

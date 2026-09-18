{-# LANGUAGE FlexibleInstances #-}

module DNS.Wire.Builder (
    -- * Builder
    Builder,
    WriteBuffer,
    runBuilder,

    -- ** Basic builders
    put8,
    put16,
    put32,
    put48,
    putInt8,
    putInt16,
    putInt32,
    putShortByteString,
    putLenShortByteString,

    -- ** Lower utilities
    with16Length,

    -- ** Builder state
    BState,
    position,
    pushPointer,
    popPointer,
) where

import qualified Data.ByteString.Short as Short
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Network.ByteOrder
import System.IO.Unsafe (unsafeDupablePerformIO)

import DNS.Types.Imports
import DNS.Wire.Types

----------------------------------------------------------------

-- | Builder type
type Builder a = WriteBuffer -> IORef BState -> IO a

----------------------------------------------------------------

-- | Builder state
newtype BState = BState
    { bstDomain :: Map WireLabels Int
    }

initialBState :: BState
initialBState = BState M.empty

----------------------------------------------------------------

runBuilder :: Int -> Builder () -> ByteString
runBuilder len builder = unsafeDupablePerformIO $ do
    withWriteBuffer len $ \wbuf -> do
        ref <- newIORef initialBState
        builder wbuf ref

----------------------------------------------------------------

maxCompressionPerMsg :: Int
maxCompressionPerMsg = 128

pushPointer :: WireLabels -> Int -> IORef BState -> IO ()
pushPointer dom pos ref = do
    BState m <- readIORef ref
    when (M.size m >= maxCompressionPerMsg) $ do
        let info = unwords $ map show $ dom : take 16 (M.keys m)
        fail ("Wire.Builder: pushPointer: compression limit exceeded: " ++ info ++ " ...")
    let m' = M.insert dom pos m
    writeIORef ref $ BState m'

popPointer :: WireLabels -> IORef BState -> IO (Maybe Int)
popPointer dom ref = M.lookup dom . bstDomain <$> readIORef ref

----------------------------------------------------------------

integralCast :: (Show a, Integral a, Integral b) => String -> a -> IO b
integralCast err src
    | toInteger src == toInteger dst = return dst
    | otherwise = fail (err ++ ": out-of-range: " ++ show src)
  where
    dst = fromIntegral src
{-# INLINEABLE integralCast #-}

----------------------------------------------------------------

put8 :: WriteBuffer -> Word8 -> IO ()
put8 = write8

put16 :: WriteBuffer -> Word16 -> IO ()
put16 = write16

put32 :: WriteBuffer -> Word32 -> IO ()
put32 = write32

-- | Writing the low 48 bits of a 'Word64', most significant octet
--   first.  DNS uses a 48 bit integer for a time, in TSIG.
put48 :: WriteBuffer -> Word64 -> IO ()
put48 wbuf w = do
    put16 wbuf $ fromIntegral (w `shiftR` 32)
    put32 wbuf $ fromIntegral w

putInt8 :: WriteBuffer -> Int -> IO ()
putInt8 wbuf n = write8 wbuf =<< integralCast "putInt8" n

putInt16 :: WriteBuffer -> Int -> IO ()
putInt16 wbuf n = write16 wbuf =<< integralCast "putInt16" n

putInt32 :: WriteBuffer -> Int -> IO ()
putInt32 wbuf n = write32 wbuf =<< integralCast "putInt32" n

----------------------------------------------------------------

putShortByteString :: WriteBuffer -> ShortByteString -> IO ()
putShortByteString = copyShortByteString

-- In the case of the TXT record, we need to put the string length
putLenShortByteString :: WriteBuffer -> ShortByteString -> IO ()
putLenShortByteString wbuf txt = do
    len <- integralCast "putLenShortByteString" $ Short.length txt
    write8 wbuf len
    putShortByteString wbuf txt

with16Length :: Builder () -> Builder ()
with16Length builder wbuf ref = do
    save wbuf
    write16 wbuf 0 -- reserve space
    beg <- position wbuf
    builder wbuf ref
    end <- position wbuf
    let len = end - beg
    goBack wbuf
    write16 wbuf =<< integralCast "with16Length" len
    ff wbuf len

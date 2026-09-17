{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module DNS.Wire.Parser (
    -- * Parser
    Parser,
    ReadBuffer,
    runParser,
    runParserAt,
    failParser,

    -- ** Basic parsers
    get8,
    get16,
    get32,
    get48,
    getInt8,
    getInt16,
    getInt32,
    getNByteString,
    getNShortByteString,
    sGetMany,
    getNBytes,
    getNOctets,
    skipNBytes,

    -- ** Parser state
    PState,
    position,
    pushDomain,
    popDomain,
    getAtTime,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as Short
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Network.ByteOrder
import System.IO.Unsafe (unsafeDupablePerformIO)

import DNS.Types.Error
import DNS.Types.Imports
import DNS.Types.Time
import DNS.Wire.Types

----------------------------------------------------------------

-- | Parser type
type Parser a = ReadBuffer -> IORef PState -> IO a

-- | Parser state
data PState = PState
    { pstDomain :: IntMap Labels
    , pstAtTime :: EpochTime
    }
    deriving (Eq, Show)

initialState :: EpochTime -> PState
initialState t = PState IM.empty t

----------------------------------------------------------------

pushDomain :: IM.Key -> Labels -> IORef PState -> IO ()
pushDomain n d ref = do
    PState dom t <- readIORef ref
    writeIORef ref $ PState (IM.insert n d dom) t

popDomain :: IM.Key -> IORef PState -> IO (Maybe Labels)
popDomain n ref = IM.lookup n . pstDomain <$> readIORef ref

getAtTime :: IORef PState -> IO EpochTime
getAtTime ref = pstAtTime <$> readIORef ref

----------------------------------------------------------------

get8 :: ReadBuffer -> IO Word8
get8 = read8

get16 :: ReadBuffer -> IO Word16
get16 = read16

get32 :: ReadBuffer -> IO Word32
get32 = read32

-- | Reading a 48 bit integer, most significant octet first.
get48 :: ReadBuffer -> IO Word64
get48 rbuf = do
    hi <- get16 rbuf
    lo <- get32 rbuf
    return $ (fromIntegral hi `shiftL` 32) .|. fromIntegral lo

getInt8 :: ReadBuffer -> IO Int
getInt8 rbuf = fromIntegral <$> get8 rbuf

getInt16 :: ReadBuffer -> IO Int
getInt16 rbuf = fromIntegral <$> get16 rbuf

getInt32 :: ReadBuffer -> IO Int
getInt32 rbuf = fromIntegral <$> get32 rbuf

----------------------------------------------------------------

getNBytes :: ReadBuffer -> Int -> IO [Int]
getNBytes rbuf n
    | n < 0 = failParser "getNBytes: malformed or truncated input"
    | otherwise = toInts <$> extractByteString rbuf n
  where
    toInts = map fromIntegral . BS.unpack

getNOctets :: ReadBuffer -> Int -> IO [Word8]
getNOctets rbuf n
    | n < 0 = failParser "getNOctets: malformed or truncated input"
    | otherwise = BS.unpack <$> extractByteString rbuf n

skipNBytes :: ReadBuffer -> Int -> IO ()
skipNBytes rbuf n
    | n < 0 = failParser "skipNBytes: malformed or truncated input"
    | otherwise = ff rbuf n

getNByteString :: ReadBuffer -> Int -> IO ByteString
getNByteString rbuf n
    | n < 0 = failParser "getNByteString: malformed or truncated input"
    | otherwise = extractByteString rbuf n

getNShortByteString :: ReadBuffer -> Int -> IO ShortByteString
getNShortByteString rbuf n
    | n < 0 = failParser "getNShortByteString: malformed or truncated input"
    | otherwise = Short.toShort <$> extractByteString rbuf n

-- | Parse a list of elements that takes up exactly a given number of bytes.
-- In order to avoid infinite loops, if an element parser succeeds without
-- moving the buffer offset forward, an error will be returned.
sGetMany
    :: String
    -- ^ element type for error messages
    -> Int
    -- ^ input buffer length
    -> Parser a
    -- ^ element parser
    -> Parser [a]
sGetMany elemname len parser = \rbuf ref -> do
    lim <- (+ len) <$> position rbuf
    go lim id rbuf ref
  where
    go lim build rbuf ref = do
        pos <- position rbuf
        case pos `compare` lim of
            EQ -> return $ build []
            LT -> do
                x <- parser rbuf ref
                go lim (build . (x :)) rbuf ref
            GT -> failParser $ "sGetMany: internal error: in-place success for " ++ elemname

----------------------------------------------------------------

-- | To get a broad range of correct RRSIG inception and expiration times
-- without over or underflow, we choose a time half way between midnight PDT
-- 2010-07-15 (the day the root zone was signed) and 2^32 seconds later on
-- 2146-08-21.  Since 'decode' and 'runParser' are pure, we can't peek at the
-- current time while parsing.  Outside this date range the output is off by
-- some non-zero multiple 2\^32 seconds.
dnsTimeMid :: EpochTime
dnsTimeMid = 3426660848

failParser :: String -> IO a
failParser = E.throwIO . DecodeError

runParserAt :: EpochTime -> Parser a -> ByteString -> Either DNSError a
runParserAt t parser inp =
    unsafeDupablePerformIO $ E.handle handler parse
  where
    parse = withReadBuffer inp $ \rbuf -> do
        ref <- newIORef $ initialState t
        ret <- parser rbuf ref
        left <- remainingSize rbuf
        when (left /= 0) $ failParser "excess input"
        return $ Right ret
    -- SomeException: asynchronous exceptions are re-thrown
    handler se@(E.SomeException e)
        | Just (DecodeError msg) <- E.fromException se = return $ Left $ DecodeError msg
        | Just (E.SomeAsyncException _) <- E.fromException se = E.throwIO se
        | otherwise = return $ Left $ DecodeError $ "incomplete input: " ++ show e

runParser :: Parser a -> ByteString -> Either DNSError a
runParser = runParserAt dnsTimeMid

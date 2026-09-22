-- | The parts of an outgoing query which are there to be hard to guess.
--
--   A resolver which does not validate has the identifier, the source
--   port and -- where it mixes the case of the name it asks about -- a
--   bit for each letter of that name, between it and anybody who cares
--   to answer in the real server's place.  RFC 5452 Sec 9 is about
--   making all of them cost an attacker as much as possible to guess,
--   and sixteen bits of identifier is not much to begin with.
--
--   So the bits come from a stream cipher rather than from a fast
--   non-cryptographic generator: a generator whose next number can be
--   worked out from the ones already seen leaves nothing at all, and
--   every identifier a resolver sends has been seen by whoever it was
--   sent to.  Taking two bytes of ChaCha at a time costs about nine
--   times what a non-cryptographic generator did, which is the cipher
--   being set up for each draw; taking a few hundred bytes and handing
--   them out as they are wanted costs about a fifth more, which is
--   nothing beside sending a packet.
module DNS.Do53.Id (
    singleGenId,
    newConcurrentGenId,
    newConcurrentMixCase,
    mixCase,
    sameCase,
)
where

import Control.Concurrent
import Control.Monad
import Crypto.Random (ChaChaDRG, drgNew, randomBytesGenerate)
import DNS.Types
import Data.Array
import Data.Bits (shiftL, testBit, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as Short
import Data.IORef
import Data.List (mapAccumL)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

-- | How much of the stream to take at once.  Past a few hundred bytes
--   the setting up stops showing, so this is where the curve flattens
--   rather than as much as could be held.
chunkSize :: Int
chunkSize = 512

-- | A generator, and what is left of the last chunk it was asked for.
type Gen = IORef (BS.ByteString, ChaChaDRG)

newGen :: IO Gen
newGen = newIORef . (,) BS.empty =<< drgNew

-- | The next so many bytes, taken from the chunk in hand or from a
--   fresh one.  Two threads on the same generator are ordered by the
--   modify rather than left to take the same bytes.
nextBytes :: Int -> Gen -> IO BS.ByteString
nextBytes n ref = atomicModifyIORef' ref step
  where
    step (rest, g)
        | BS.length rest >= n = ((BS.drop n rest, g), BS.take n rest)
        | otherwise =
            let (fresh, g') = randomBytesGenerate (max chunkSize n) g
             in ((BS.drop n fresh, g'), BS.take n fresh)

word16 :: BS.ByteString -> Identifier
word16 = BS.foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0

{-# NOINLINE globalGen #-}
globalGen :: Gen
globalGen = unsafePerformIO newGen

-- | Identifiers from one generator, for a caller with no reason to want
--   more than one.
singleGenId :: IO Identifier
singleGenId = word16 <$> nextBytes 2 globalGen

-- | A generator for each capability, so that threads running at the
--   same time are not queueing behind each other for the same one, and
--   an action which draws the bytes asked for from whichever one
--   belongs to the thread asking.
newConcurrentBytes :: IO (Int -> IO BS.ByteString)
newConcurrentBytes = do
    n <- getNumCapabilities
    gs <- replicateM n newGen
    let arr = listArray (0, n - 1) gs
    return $ \k -> do
        (i, _) <- myThreadId >>= threadCapability
        nextBytes k (arr ! i)

newConcurrentGenId :: IO (IO Identifier)
newConcurrentGenId = (\draw -> word16 <$> draw 2) <$> newConcurrentBytes

-- | Mixing the case of a name, ready to be put in @ractionMixCase@.
newConcurrentMixCase :: IO (Domain -> IO Domain)
newConcurrentMixCase = flip mixCase <$> newConcurrentBytes

-- | The name with the case of each of its letters chosen at random.
--
--   It is the same name: 'Eq' and 'Ord' fold case, so nothing compares,
--   orders or is looked up differently for having been mixed.  Only
--   what goes on the wire changes, RFC 4343 Sec 3 asking a server to
--   give the name back as it was given.  An answer which does not is
--   not an answer to this query, and whoever sent it had to guess a bit
--   for every letter to make it look like one.
--
--   A bit which is not set asks for upper case, so a generator of
--   noughts writes the name in upper case throughout and one of ones
--   writes it in lower.
mixCase :: Domain -> (Int -> IO BS.ByteString) -> IO Domain
mixCase dom draw = do
    bits <- unpackBits <$> draw ((sum (map letters labels) + 7) `div` 8)
    pure $ fromWireLabels $ snd $ mapAccumL mixLabel bits labels
  where
    labels = originalWireLabels dom
    unpackBits = concatMap (\w -> [testBit w i | i <- [0 .. 7]]) . BS.unpack

-- | Whether two names are written the same way and not merely the same
--   name.  This is what an echo is checked with, 'Eq' being no use for
--   it by design.
sameCase :: Domain -> Domain -> Bool
sameCase d0 d1 = originalWireLabels d0 == originalWireLabels d1

mixLabel :: [Bool] -> Short.ShortByteString -> ([Bool], Short.ShortByteString)
mixLabel bs0 l = Short.pack <$> mapAccumL one bs0 (Short.unpack l)
  where
    one (b : bs) w | isLetter w = (bs, if b then w .|. 0x20 else w .&. 0xDF)
    one bs w = (bs, w)

letters :: Short.ShortByteString -> Int
letters = length . filter isLetter . Short.unpack

-- | A byte with a case to mix.  Only the twenty-six, whatever a name
--   may hold otherwise: RFC 4343 Sec 3 has case folding reach no
--   further than ASCII, so nothing else has an upper and a lower form
--   to choose between.
isLetter :: Word8 -> Bool
isLetter w = (w .&. 0xDF) - 0x41 < 26

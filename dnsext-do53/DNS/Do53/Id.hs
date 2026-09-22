-- | Where the identifier on an outgoing query comes from.
--
--   A resolver which does not validate has sixteen bits of identifier
--   and the source port between it and anybody who cares to answer in
--   the real server's place, and RFC 5452 Sec 9 is about making both of
--   them cost an attacker as much as possible to guess.  Sixteen bits
--   is not much to begin with; a generator whose next number can be
--   worked out from the ones already seen leaves nothing at all, and
--   every identifier a resolver sends has been seen by whoever it was
--   sent to.
--
--   So the numbers come from a stream cipher rather than from a fast
--   non-cryptographic generator.  Taking two bytes of ChaCha at a time
--   costs about nine times what the old generator did, which is the
--   cipher being set up for each identifier; taking a few hundred bytes
--   and handing them out two at a time costs about a fifth more than
--   the old generator, which is nothing beside sending a packet.
module DNS.Do53.Id (
    singleGenId,
    newConcurrentGenId,
)
where

import Control.Concurrent
import Control.Monad
import Crypto.Random (ChaChaDRG, drgNew, randomBytesGenerate)
import DNS.Types
import Data.Array
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.IORef
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

-- | The next identifier, taken from the chunk in hand or from a fresh
--   one.  Two threads on the same generator are ordered by the modify
--   rather than left to take the same two bytes.
nextId :: Gen -> IO Identifier
nextId ref = atomicModifyIORef' ref step
  where
    step (rest, g)
        | BS.length rest >= 2 = ((BS.drop 2 rest, g), word16 rest)
        | otherwise =
            let (fresh, g') = randomBytesGenerate chunkSize g
             in ((BS.drop 2 fresh, g'), word16 fresh)
    word16 = BS.foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 . BS.take 2

{-# NOINLINE globalGen #-}
globalGen :: Gen
globalGen = unsafePerformIO newGen

-- | Identifiers from one generator, for a caller with no reason to want
--   more than one.
singleGenId :: IO Identifier
singleGenId = nextId globalGen

-- | Identifiers from a generator for each capability, so that threads
--   running at the same time are not queueing behind each other for the
--   same one.
newConcurrentGenId :: IO (IO Identifier)
newConcurrentGenId = do
    n <- getNumCapabilities
    gs <- replicateM n newGen
    let arr = listArray (0, n - 1) gs
    return $ do
        (i, _) <- myThreadId >>= threadCapability
        nextId (arr ! i)

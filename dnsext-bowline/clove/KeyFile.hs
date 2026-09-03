{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module KeyFile where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.UnixTime
import System.Directory
import System.FilePath

import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
saveKSKInfo :: KeyInfo -> IO ()
saveKSKInfo KeyInfo{..} = do
    let zoneDir = init $ toRepresentation keyInfoZone
    createDirectoryIfMissing True zoneDir
    let statusBS =
            "Zone:       " <> toRepresentation keyInfoZone <> "\n" <>
            "KeyTag:     " <> toB keyInfoTag <> "\n" <>
            "Algorithm:  " <> toB (fromPubAlg keyInfoAlgorithm)  <> " # " <> toB keyInfoAlgorithm <> "\n" <>
            "DigestAlgo: " <> toB (fromDigestAlg keyInfoDigestAlgo) <> " # " <> toB keyInfoDigestAlgo <> "\n" <>
            "Digest:     " <> Opaque.toBase16 keyInfoDigest <> "\n" <>
            "PublicKey:  " <> Opaque.toBase16 (fromPubKey keyInfoPubKey) <> "\n" <>
            "PrivateKey: " <> B16.encode keyInfoPriKey <> "\n" <>
            "Flag:       " <> toB keyInfoFlag <> "\n"
    t <- getUnixTime
    fn <- C8.unpack <$> formatUnixTime "%Y-%m-%d-%H:%M:%S.ksk" t
    C8.writeFile (zoneDir </> fn) statusBS
  where
    toB :: Show a => a -> C8.ByteString
    toB = C8.pack . show
{- FOURMOLU_ENABLE -}

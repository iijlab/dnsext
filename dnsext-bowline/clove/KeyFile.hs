{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module KeyFile where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.UnixTime
import System.Directory
import System.FilePath
import System.IO.Error (ioeGetErrorString, ioeSetErrorString, tryIOError)

import DNS.Config
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

----------------------------------------------------------------

fromKeyInfoConf :: KeyInfoConf -> KeyInfo
fromKeyInfoConf KeyInfoConf{..} =
    KeyInfo
        { keyInfoZone = fromRepresentation kic_zone
        , keyInfoAlgorithm = toPubAlg $ fromIntegral kic_algorithm
        , keyInfoDigestAlgo = toDigestAlg $ fromIntegral kic_digest_alg
        , keyInfoTag = fromIntegral kic_keytag
        , keyInfoDigest = either (const "") id $ Opaque.fromBase16 $ C8.pack kic_digest
        , keyInfoPubKey = either (const $ toPubKey "") toPubKey $ Opaque.fromBase16 $ C8.pack kic_public_key
        , keyInfoPriKey = B16.decodeLenient $ C8.pack kic_private_key
        , keyInfoFlag = fromIntegral kic_flag
        }

{- FOURMOLU_DISABLE -}
data KeyInfoConf = KeyInfoConf
    { kic_zone        :: String
    , kic_keytag      :: Int
    , kic_algorithm   :: Int
    , kic_digest_alg  :: Int
    , kic_digest      :: String
    , kic_public_key  :: String
    , kic_private_key :: String
    , kic_flag        :: Int
    }
    deriving (Show)

defaultKeyInfoConf :: KeyInfoConf
defaultKeyInfoConf =
    KeyInfoConf
        { kic_zone        = ""
        , kic_keytag      = 0
        , kic_algorithm   = 0
        , kic_digest_alg  = 0
        , kic_digest      = ""
        , kic_public_key  = ""
        , kic_private_key = ""
        , kic_flag        = 0
        }
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
makeKeyInfoConf :: KeyInfoConf -> [Conf] -> IO KeyInfoConf
makeKeyInfoConf def conf = do
    kic_zone        <- get "Zone"       kic_zone
    kic_keytag      <- get "KeyTag"     kic_keytag
    kic_algorithm   <- get "Algorithm"  kic_algorithm
    kic_digest_alg  <- get "DigestAlgo" kic_digest_alg
    kic_digest      <- get "Digest"     kic_digest
    kic_public_key  <- get "PublicKey"  kic_public_key
    kic_private_key <- get "PrivateKey" kic_private_key
    kic_flag        <- get "Flag"       kic_flag
    pure KeyInfoConf{..}
  where
    get k func = do
        et <- tryIOError $ maybe (pure $ func def) fromConf $ lookup k conf
        let left e = do
                let e' = ioeSetErrorString e (k ++ ": " ++ ioeGetErrorString e)
                ioError e'
        either left pure et
{- FOURMOLU_ENABLE -}

loadKSKInfo :: FilePath -> IO KeyInfo
loadKSKInfo fn = do
    cnf <- loadFile fn
    kic <- makeKeyInfoConf defaultKeyInfoConf cnf
    return $ fromKeyInfoConf kic

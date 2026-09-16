{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module KeyFile where

import Control.Monad
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.List (isSuffixOf, sortBy)
import Data.UnixTime
import Foreign.C.Types
import System.Directory
import System.FilePath
import qualified System.IO.Error as E
import System.Posix.Files

import DNS.Config
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
saveKSKInfo :: FilePath -> KeyInfo -> IO ()
saveKSKInfo zoneDir ki = save zoneDir ".ksk" ki

saveZSKInfo :: FilePath -> KeyInfo -> IO ()
saveZSKInfo zoneDir ki = save zoneDir ".zsk" ki

save :: FilePath -> String -> KeyInfo -> IO ()
save zoneDir suffix KeyInfo{..} = do
    fn <- getUnixTime >>= findNonExistingFile zoneDir suffix
    C8.writeFile fn statusBS
  where
    statusBS =
        "Zone:       " <> toRepresentation keyInfoZone <> "\n" <>
        "KeyTag:     " <> toB keyInfoTag <> "\n" <>
        "Algorithm:  " <> toB (fromPubAlg keyInfoAlgorithm)  <> " # " <> toB keyInfoAlgorithm <> "\n" <>
        "DigestAlgo: " <> toB (fromDigestAlg keyInfoDigestAlgo) <> " # " <> toB keyInfoDigestAlgo <> "\n" <>
        "Digest:     " <> Opaque.toBase16 keyInfoDigest <> "\n" <>
        "PublicKey:  " <> Opaque.toBase16 (fromPubKey keyInfoPubKey) <> "\n" <>
        "PrivateKey: " <> B16.encode keyInfoPriKey <> "\n" <>
        "Flag:       " <> toB keyInfoFlag <> "\n"
    toB :: Show a => a -> C8.ByteString
    toB = C8.pack . show

findNonExistingFile :: FilePath -> String -> UnixTime -> IO FilePath
findNonExistingFile zoneDir suffix ut0 = loop ut0
  where
    loop ut = do
        fn <- C8.unpack <$> formatUnixTime ("%Y-%m-%d-%H:%M:%S" <> C8.pack suffix) ut
        let afn = zoneDir </> fn
        exist <- doesFileExist afn
        if exist then loop ut {utSeconds = utSeconds ut + 1}
            else return afn
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

loadKSKInfo
    :: FilePath
    -> KeyConfig
    -> IO (KeyInfo, ResourceRecord)
loadKSKInfo zoneDir keyConf = do
    ksks <- filter (".ksk" `isSuffixOf`) <$> listDirectory zoneDir
    case sortBy (flip compare) ksks of -- decreasing order
        [] -> generateKey zoneDir keyConf
        fn : _ -> loadKey zoneDir keyConf fn

loadZSKInfo
    :: FilePath
    -> KeyConfig
    -> IO
        ( (KeyInfo, ResourceRecord) -- previous
        , (KeyInfo, ResourceRecord) -- current
        , (KeyInfo, ResourceRecord) -- next
        )
loadZSKInfo zoneDir keyConf = do
    ksks <- filter (".zsk" `isSuffixOf`) <$> listDirectory zoneDir
    case sortBy (flip compare) ksks of -- decreasing order
        fn2 : fn1 : fn0 : _ -> do
            ki0 <- loadKey zoneDir keyConf fn0
            ki1 <- loadKey zoneDir keyConf fn1
            ki2 <- loadKey zoneDir keyConf fn2
            return (ki0, ki1, ki2)
        _ -> do
            ki0 <- generateKey zoneDir keyConf
            ki1 <- generateKey zoneDir keyConf
            ki2 <- generateKey zoneDir keyConf
            return (ki0, ki1, ki2)

-- | Loading an existing key.  A key file which cannot be read is an
--   error: generating a new key instead would silently roll the key over
--   and, for a KSK, break the chain of trust at the parent.
loadKey
    :: FilePath
    -> KeyConfig
    -> FilePath
    -> IO (KeyInfo, ResourceRecord)
loadKey zoneDir keyConf fn = do
    ki <- loadKeyInfo (zoneDir </> fn)
    let (_, _, dnskeyrr, _) = fromKeyInfo ki $ keyConfTTL keyConf
    return (ki, dnskeyrr)

generateKey
    :: FilePath
    -> KeyConfig
    -> IO (KeyInfo, ResourceRecord) -- DNSKEY
generateKey zoneDir keyConf = do
    (keyInfo, dnskeyrr, _) <- generateKeyInfo keyConf
    case keyConfType keyConf of
        KSK -> saveKSKInfo zoneDir keyInfo
        ZSK -> saveZSKInfo zoneDir keyInfo
    return (keyInfo, dnskeyrr)

----------------------------------------------------------------

rolloverZSK :: FilePath -> KeyConfig -> IO ()
rolloverZSK zoneDir keyConf = do
    ksks <- filter (".zsk" `isSuffixOf`) <$> listDirectory zoneDir
    case sortBy (flip compare) ksks of -- decreasing order
        [] -> E.ioError $ E.userError "no ZSK files are found"
        fn : _ -> do
            ut0 <- fromEpochTime . modificationTime <$> getFileStatus (zoneDir </> fn)
            ut1 <- getUnixTime
            let CTime diff = udtSeconds (ut1 `diffUnixTime` ut0)
            when (diff + 300 > fromDNSTime (keyConfLifetime keyConf)) $ do
                -- fixme: 5min good enough?
                void $ generateKey zoneDir keyConf

----------------------------------------------------------------

fromKeyInfoConf :: KeyInfoConf -> Either String KeyInfo
fromKeyInfoConf KeyInfoConf{..} = do
    let keyInfoZone = fromRepresentation kic_zone
        keyInfoAlgorithm = toPubAlg $ fromIntegral kic_algorithm
        keyInfoDigestAlgo = toDigestAlg $ fromIntegral kic_digest_alg
        keyInfoTag = fromIntegral kic_keytag
    keyInfoDigest <- Opaque.fromBase16 $ C8.pack kic_digest
    keyInfoPubKey <- toPubKey <$> Opaque.fromBase16 (C8.pack kic_public_key)
    -- Not decodeLenient: a garbled private key must not be accepted.
    keyInfoPriKey <- B16.decode $ C8.pack kic_private_key
    let keyInfoFlag = fromIntegral kic_flag
    return $ KeyInfo{..}

{- FOURMOLU_DISABLE -}
-- | Every field is mandatory.  There is no sensible default for any of
--   them: an absent field used to become algorithm 0 or an empty private
--   key, which produced a 'KeyInfo' that signed nothing.
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

{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
makeKeyInfoConf :: [Conf] -> IO KeyInfoConf
makeKeyInfoConf conf = do
    kic_zone        <- get "Zone"
    kic_keytag      <- get "KeyTag"
    kic_algorithm   <- get "Algorithm"
    kic_digest_alg  <- get "DigestAlgo"
    kic_digest      <- get "Digest"
    kic_public_key  <- get "PublicKey"
    kic_private_key <- get "PrivateKey"
    kic_flag        <- get "Flag"
    pure KeyInfoConf{..}
  where
    get k = case lookup k conf of
        Nothing -> E.ioError $ E.userError $ k ++ ": missing"
        Just v  -> do
            et <- E.tryIOError $ fromConf v
            let left e = do
                    let e' = E.ioeSetErrorString e (k ++ ": " ++ E.ioeGetErrorString e)
                    E.ioError e'
            either left pure et
{- FOURMOLU_ENABLE -}

loadKeyInfo :: FilePath -> IO KeyInfo
loadKeyInfo fn = do
    cnf <- loadFile fn
    kic <- makeKeyInfoConf cnf
    case fromKeyInfoConf kic of
        Left e -> E.ioError $ E.userError $ fn ++ ": " ++ e
        Right ki -> return ki

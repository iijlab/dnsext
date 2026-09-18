{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module KeyFile where

import qualified Control.Exception as E
import Control.Monad
import Data.Bits (shiftR)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.List (isSuffixOf, sort, sortBy)
import Data.UnixTime
import Foreign.C.Types
import System.Directory
import System.FilePath
import qualified System.IO.Error as E
import System.Posix.Files

import AtomicFile
import DNS.Config
import DNS.Log
import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import Types

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
saveKSKInfo :: FilePath -> KeyInfo -> IO ()
saveKSKInfo zoneDir ki = save zoneDir ".ksk" ki

saveZSKInfo :: FilePath -> KeyInfo -> IO ()
saveZSKInfo zoneDir ki = save zoneDir ".zsk" ki

save :: FilePath -> String -> KeyInfo -> IO ()
save zoneDir suffix KeyInfo{..} = do
    fn <- getUnixTime >>= findNonExistingFile zoneDir suffix
    -- Private: this holds a private key.
    writeAtomic fn 0o600 statusBS
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

-- | Whether the zone is starting out, that is whether its directory
--   holds no key of either kind.  Asked before either kind is loaded,
--   since loading one makes one where there is none, and a key made a
--   moment ago says nothing about what was there before.
startingOut :: FilePath -> IO Bool
startingOut zoneDir = null <$> ((++) <$> keyFiles zoneDir ".ksk" <*> keyFiles zoneDir ".zsk")

-- | The KSK of the zone, made if there is none.
--
--   A zone which has run before has keys, so a missing KSK in a
--   directory which holds some is a KSK which has gone rather than one
--   which was never there.  A new one is made -- there is nothing else
--   to be done with a key whose file is gone -- but it will not match
--   the DS the parent holds, and until a new DS is published there the
--   zone is bogus to every validator, not only to those which cached
--   something.  Hence the warning.
loadKSKInfo
    :: Env
    -> Bool
    -- ^ whether the zone is starting out, and so has no key to miss
    -> FilePath
    -> KeyConfig
    -> IO (KeyInfo, ResourceRecord)
loadKSKInfo env fresh zoneDir keyConf = do
    ksks <- keyFiles zoneDir ".ksk"
    case sortBy (flip compare) ksks of -- decreasing order
        [] -> do
            unless fresh $
                warn env keyConf $
                    "the KSK is gone, so a new one is being made."
                        ++ "  The zone stays bogus until the DS of the new key is published at the parent."
            generateKSK zoneDir keyConf
        fn : _ -> loadKey zoneDir keyConf fn

loadZSKInfo
    :: Env
    -> Bool
    -- ^ whether the zone is starting out, and so has no keys to miss
    -> FilePath
    -> Int
    -> KeyConfig
    -> IO
        ( (KeyInfo, ResourceRecord) -- previous
        , (KeyInfo, ResourceRecord) -- current
        , (KeyInfo, ResourceRecord) -- next
        )
loadZSKInfo env fresh zoneDir preserve keyConf = do
    zsks <- keyFiles zoneDir ".zsk"
    case sortBy (flip compare) zsks of -- decreasing order
        fn2 : fn1 : fn0 : _ -> do
            ki0 <- loadKey zoneDir keyConf fn0
            ki1 <- loadKey zoneDir keyConf fn1
            ki2 <- loadKey zoneDir keyConf fn2
            return (ki0, ki1, ki2)
        -- Fewer than the three which are published: some have gone
        -- missing, since nothing here leaves one or two.  A whole new
        -- set is made, which is valid DNSSEC -- the KSK has not changed,
        -- so the DS still matches -- but a resolver holding the old
        -- DNSKEY RRset cannot check what the new keys sign until that
        -- RRset expires from its cache.  Worth a sentence rather than
        -- the silence it had.
        found -> do
            unless fresh $
                warn env keyConf $
                    show (length found)
                        ++ " ZSK(s) where three are published, so a new set of three is being made."
                        ++ "  Resolvers holding the old DNSKEY RRset cannot validate until it expires from their caches."
            ki0 <- generateZSK zoneDir preserve keyConf
            ki1 <- generateZSK zoneDir preserve keyConf
            ki2 <- generateZSK zoneDir preserve keyConf
            return (ki0, ki1, ki2)

-- | The key files of one kind a zone has.
keyFiles :: FilePath -> String -> IO [FilePath]
keyFiles zoneDir suffix = filter (suffix `isSuffixOf`) <$> listDirectory zoneDir

-- | Saying which zone a warning is about, as the rest of the log does.
warn :: Env -> KeyConfig -> String -> IO ()
warn env keyConf msg = envPutLines env WARNING Nothing [zoneLabel (keyConfZone keyConf) ++ msg]

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

generateKSK
    :: FilePath
    -> KeyConfig
    -> IO (KeyInfo, ResourceRecord) -- DNSKEY
generateKSK zoneDir keyConf = do
    (keyInfo, dnskeyrr, _) <- generateKeyInfo keyConf
    saveKSKInfo zoneDir keyInfo
    return (keyInfo, dnskeyrr)

-- | Generating the next ZSK, keeping at most 'preserve' of them on
--   disk.  Each one holds a private key, and only the three newest are
--   ever published, so they must not pile up for ever.
generateZSK
    :: FilePath
    -> Int
    -> KeyConfig
    -> IO (KeyInfo, ResourceRecord) -- DNSKEY
generateZSK zoneDir preserve keyConf = do
    (keyInfo, dnskeyrr, _) <- generateKeyInfo keyConf
    saveZSKInfo zoneDir keyInfo
    -- Pruned once the new key is safely stored, never before it.
    pruneZSK zoneDir preserve
    return (keyInfo, dnskeyrr)

-- | Removing the oldest ZSKs until at most 'keep' are left.  A key file
--   is named after the time it was made, so sorting by name sorts by
--   age.
pruneZSK :: FilePath -> Int -> IO ()
pruneZSK zoneDir keep = do
    zsks <- keyFiles zoneDir ".zsk"
    mapM_ (removeFile . (zoneDir </>)) $ take (length zsks - keep) $ sort zsks

----------------------------------------------------------------

-- | How early a rollover may happen, so that waking a moment before the
--   key is due does not put the rollover off for another whole
--   duration.  A sixty-fourth of the duration: a margin of a fixed
--   number of seconds is either nothing at all next to a long duration,
--   or the whole of a short one -- at which point every wake up
--   generates a key.
rolloverMargin :: Int -> Int
rolloverMargin duration = duration `shiftR` 6

-- | Generating the next ZSK once the newest one has been in use for
--   the rollover duration.  That duration is how long a key is used,
--   which is a different thing from how long an RRSIG stays valid.
rolloverZSK :: FilePath -> Int -> Int -> KeyConfig -> IO ()
rolloverZSK zoneDir duration preserve keyConf = do
    zsks <- keyFiles zoneDir ".zsk"
    case sortBy (flip compare) zsks of -- decreasing order
    -- No key to roll over yet.  Not an error: loading the zone is
    -- what creates the first set, and it may not have got that far.
        [] -> return ()
        fn : _ -> do
            ut0 <- fromEpochTime . modificationTime <$> getFileStatus (zoneDir </> fn)
            ut1 <- getUnixTime
            let CTime diff = udtSeconds (ut1 `diffUnixTime` ut0)
                margin = fromIntegral $ rolloverMargin duration
            when (diff + margin > fromIntegral duration) $
                void $
                    generateZSK zoneDir preserve keyConf

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

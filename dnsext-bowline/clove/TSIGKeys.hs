{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Reading the secrets clove shares with its peers.
module TSIGKeys (
    TSIGKeys,
    loadTSIGKeys,
    countTSIGKeys,
    lookupTSIGKey,
    takeTSIGTime,
) where

import Control.Monad (when)
import Data.Bits ((.&.))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Data.List (nub, (\\))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word64)
import System.Directory (doesFileExist)
import qualified System.IO.Error as E
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Types (FileMode)

import DNS.Config
import DNS.Log
import DNS.TSIG
import DNS.Types

import Config
import Types

----------------------------------------------------------------

-- | The keys clove holds, by the name their peers call them, with a
--   note of the last time a message was taken under each of them.
--
--   One note for the whole server: a key is used over UDP and over TCP
--   alike, and a message put back on either is the same message put
--   back.
data TSIGKeys = TSIGKeys
    { keysByName :: Map Domain TSIGKey
    , timesTaken :: IORef (Map Domain Word64)
    }

-- | Finding a key by name.
lookupTSIGKey :: Domain -> TSIGKeys -> Maybe TSIGKey
lookupTSIGKey name = M.lookup name . keysByName

-- | How many keys there are, for the log to say.
countTSIGKeys :: TSIGKeys -> Int
countTSIGKeys = M.size . keysByName

-- | Whether a message signed at this time is one to take under this
--   key, remembering it where it is (RFC 8945 Sec 5.2.3).
--
--   A message which arrives later carrying an earlier time than the last
--   one taken is an older message put back, or a clock which has gone
--   backwards, and either way it is refused.  The RFC asks for that and
--   for no more: the same time over again is taken, so a message which
--   is put back before anything newer has been seen goes through, and a
--   peer signing two messages inside one second is not turned away.
--
--   The table holds one entry for each key clove was given, since a name
--   which is not one of those never gets this far.
takeTSIGTime :: TSIGKeys -> Domain -> Word64 -> IO Bool
takeTSIGTime TSIGKeys{..} name signed = atomicModifyIORef' timesTaken taking
  where
    taking taken = case M.lookup name taken of
        Just before | signed < before -> (taken, False)
        _ -> (M.insert name signed taken, True)

----------------------------------------------------------------

-- | Reading the key file, if there is one.
--
--   A missing file is no keys and no complaint: TSIG is something to be
--   turned on, and a clove which has not been given any keys works as
--   it always did.  A file which is there and cannot be read is an
--   error, since somebody meant it to be used.
loadTSIGKeys :: Env -> FilePath -> IO TSIGKeys
loadTSIGKeys env file = do
    there <- doesFileExist file
    if not there
        then held []
        else do
            checkPrivate env file
            cnf <- loadFile file
            let (before, sections) = splitConf "key" cnf
            checkNothingBefore before
            keys <- mapM makeKey sections
            checkRepeatedKey $ map tsigKeyName keys
            held keys
  where
    held keys = TSIGKeys (M.fromList [(tsigKeyName k, k) | k <- keys]) <$> newIORef M.empty

-- | A secret nobody else should be able to read.  A warning rather than
--   a refusal: the file may be fine and the mode merely untidy, and
--   stopping a server over it would be a poor trade.
checkPrivate :: Env -> FilePath -> IO ()
checkPrivate env file = do
    mode <- fileMode <$> getFileStatus file
    when (mode .&. groupOrOther /= 0) $
        envPutLines
            env
            WARNING
            Nothing
            [file ++ ": readable by others, and it holds shared secrets"]
  where
    groupOrOther = 0o077 :: FileMode

-- | Refusing to hold two keys of the same name.  One of them would
--   simply not be there, and which of the two is not something to leave
--   to the order they were written in: a message signed with the other
--   is answered BADKEY by a server which was told to hold it.
checkRepeatedKey :: [Domain] -> IO ()
checkRepeatedKey names = case nub (names \\ nub names) of
    [] -> return ()
    ds -> E.ioError $ E.userError $ "duplicate key: " ++ unwords (map toRepresentation ds)

checkNothingBefore :: [Conf] -> IO ()
checkNothingBefore [] = return ()
checkNothingBefore cnf =
    E.ioError $
        E.userError $
            "settings before the first key: " ++ unwords (map fst cnf)

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data KeyConf = KeyConf
    { kc_key       :: String
    , kc_algorithm :: String
    , kc_secret    :: String
    }

defaultKeyConf :: KeyConf
defaultKeyConf =
    KeyConf
        { kc_key       = ""
        , kc_algorithm = "hmac-sha256"
        , kc_secret    = ""
        }

makeKey :: [Conf] -> IO TSIGKey
makeKey conf = do
    ref <- newIORef []
    let get :: FromConf a => String -> (KeyConf -> a) -> IO a
        get k func = getting ref conf k func defaultKeyConf
    kc_key       <- get "key"       kc_key
    kc_algorithm <- get "algorithm" kc_algorithm
    kc_secret    <- get "secret"    kc_secret
    checkUnknown (kc_key ++ ": ") ref conf
    checkRepeated (kc_key ++ ": ") conf
    fromKeyConf KeyConf{..}
{- FOURMOLU_ENABLE -}

fromKeyConf :: KeyConf -> IO TSIGKey
fromKeyConf KeyConf{..} = do
    -- A key with no name would be held under the root, which is nobody's
    -- idea of a key name and which no zone setting could then name.
    when (null kc_key) $
        E.ioError $
            E.userError "a key section with no name: every key needs a \"key:\" of its own"
    alg <- case algorithmFromName (fromRepresentation kc_algorithm) of
        Just a -> return a
        Nothing -> failWith $ "algorithm " ++ kc_algorithm ++ " is unknown"
    secret <- case B64.decode $ C8.pack kc_secret of
        Right s | not (C8.null s) -> return s
        Right _ -> failWith "the secret is empty"
        Left e -> failWith $ "the secret is not base64: " ++ e
    return
        TSIGKey
            { tsigKeyName = fromRepresentation kc_key
            , tsigKeyAlgorithm = alg
            , tsigKeySecret = secret
            }
  where
    failWith m = E.ioError $ E.userError $ kc_key ++ ": " ++ m

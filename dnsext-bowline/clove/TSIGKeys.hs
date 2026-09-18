{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Reading the secrets clove shares with its peers.
module TSIGKeys (
    TSIGKeys,
    loadTSIGKeys,
    lookupTSIGKey,
) where

import Control.Monad (when)
import Data.Bits ((.&.))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Data.List (nub, (\\))
import Data.Map (Map)
import qualified Data.Map as M
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

-- | The keys clove holds, by the name their peers call them.
type TSIGKeys = Map Domain TSIGKey

-- | Finding a key by name.
lookupTSIGKey :: Domain -> TSIGKeys -> Maybe TSIGKey
lookupTSIGKey = M.lookup

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
        then return M.empty
        else do
            checkPrivate env file
            cnf <- loadFile file
            let (before, sections) = splitConf "key" cnf
            checkNothingBefore before
            keys <- mapM makeKey sections
            checkRepeatedKey $ map tsigKeyName keys
            return $ M.fromList [(tsigKeyName k, k) | k <- keys]

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

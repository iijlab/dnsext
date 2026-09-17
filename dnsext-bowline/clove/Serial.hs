{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serial where

import DNS.Types
import qualified Data.ByteString.Char8 as C8
import System.FilePath
import qualified System.IO.Error as E
import Text.Read

serialFile :: FilePath
serialFile = "serial"

saveSerial :: FilePath -> Serial -> IO ()
saveSerial zoneDir serial = C8.writeFile (zoneDir </> serialFile) str
  where
    str = C8.pack (show (unSerial serial)) <> "\n"

-- | Reading the stored serial.  The file is read strictly: with the lazy
--   'readFile' the handle stays open, because the digits are taken only
--   up to the newline and the rest is never demanded, and the following
--   'saveSerial' then fails with "resource busy (file is locked)".
loadSerial :: FilePath -> IO (Maybe Serial)
loadSerial zoneDir = do
    ebs <- E.tryIOError $ C8.readFile (zoneDir </> serialFile)
    case ebs of
        Left _ -> return Nothing
        Right bs -> case readMaybe $ C8.unpack $ C8.takeWhile isDigit bs of
            Nothing -> return Nothing
            Just n -> return $ Just $ Serial n
  where
    isDigit c = '0' <= c && c <= '9'

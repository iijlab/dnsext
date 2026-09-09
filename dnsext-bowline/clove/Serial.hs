{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Serial where

import DNS.Types
import System.FilePath
import qualified System.IO.Error as E
import Text.Read

serialFile :: FilePath
serialFile = "serial"

saveSerial :: FilePath -> Serial -> IO ()
saveSerial zoneDir serial = writeFile (zoneDir </> serialFile) str
  where
    str = show (unSerial serial) <> "\n"

loadSerial :: FilePath -> IO (Maybe Serial)
loadSerial zoneDir = do
    ebs <- E.tryIOError $ readFile (zoneDir </> serialFile)
    case ebs of
        Left _ -> return Nothing
        Right bs -> case readMaybe $ takeWhile (\c -> '0' <= c && c <= '9') bs of
            Nothing -> return Nothing
            Just n -> return $ Just $ Serial n

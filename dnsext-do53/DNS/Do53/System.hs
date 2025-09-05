{-# LANGUAGE CPP #-}

module DNS.Do53.System (
    getDefaultDnsServers,
)
where

import DNS.Do53.Imports

#ifdef mingw32_HOST_OS
import Foreign.C.String
import Foreign.Marshal.Alloc (allocaBytes)

foreign import ccall "getWindowsDefDnsServers" getWindowsDefDnsServers :: CString -> Int -> IO Word32

getDefaultDnsServers :: FilePath -> IO [String]
getDefaultDnsServers _ = do
  allocaBytes 256 $ \cString -> do
     res <- getWindowsDefDnsServers cString 256
     case res of
       0 -> split ',' <$> peekCString cString
       _ -> return [] -- TODO: Do proper error handling here.
  where
    split :: Char -> String -> [String]
    split c cs =
        let (h, t) = dropWhile (== c) <$> break (== c) cs
         in if null t
            then if null h then [] else [h]
            else if null h
            then split c t
            else h : split c t

#else

import Data.Char (isSpace)

getDefaultDnsServers :: FilePath -> IO [String]
getDefaultDnsServers file = toAddresses <$> readFile file
  where
    toAddresses :: String -> [String]
    toAddresses cs = map extract (filter ("nameserver" `isPrefixOf`) (lines cs))
    extract = takeWhile (not . isEnd) . dropWhile isSpace . drop 11
    isEnd = or . sequence [isSpace, (==) '#', (==) ';']

#endif

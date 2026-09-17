module AtomicFile (writeAtomic) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import System.Directory (removeFile)
import System.FilePath
import System.IO
import qualified System.IO.Error as E
import System.Posix.Files
import System.Posix.Types (FileMode)

-- | Storing a file so that it either appears complete or does not
--   appear at all.  Written to a temporary name and renamed over the
--   target, since a crash, a signal or a full disk part way through a
--   plain write leaves a truncated file under the real name, and
--   everything clove stores per zone is read back later.
--
--   The temporary is given the mode before anything is written into it,
--   and removed again if the write fails.  Its name ends in @.tmp@, so
--   a leftover is never mistaken for one of the files clove looks for.
writeAtomic :: FilePath -> FileMode -> C8.ByteString -> IO ()
writeAtomic fn mode bs = write `E.onException` discard
  where
    tmp = fn <.> "tmp"
    write = do
        withFile tmp WriteMode $ \h -> do
            setFileMode tmp mode
            C8.hPutStr h bs
        rename tmp fn
    discard = removeFile tmp `E.catchIOError` \_ -> return ()

module SocketUtil (checkDisableV6) where

import qualified Control.Exception as E
import Data.IP (IP (..), IPv6)
import qualified Data.List.NonEmpty as NE
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), SocketType (..))
import qualified Network.Socket as S
import System.IO.Error (tryIOError)

checkDisableV6 :: [IP] -> IO Bool
checkDisableV6 addrs
    | v6 : _ <- v6s = either disabled pure =<< tryIOError (checkV6 v6)
    | otherwise = pure False
  where
    v6s = [v6 | IPv6 v6 <- addrs]
    disabled e = do
        putStrLn ("disabling IPv6: " ++ show e)
        return True

checkV6 :: IPv6 -> IO Bool
checkV6 dst = do
    ai@AddrInfo{addrAddress = peer} <-
        NE.head <$> S.getAddrInfo (Just hint) (Just addr) (Just port)
    E.bracket (S.openSocket ai) S.close $ \s -> S.connect s peer
    return False
  where
    addr = show dst
    port = "53" -- "0" causes an error on BSD variants
    -- Check whether IPv6 is available by specifying `AI_ADDRCONFIG`
    -- to `addrFlags` of hints passed to `getAddrInfo`.  If `Nothing`
    -- is passed to `hints`, the default value of `addrFlags` is
    -- implementation-dependent.
    --
    -- \* Glibc: `[AI_ADDRCONFIG,AI_V4MAPPED]`.
    --   * https://man7.org/linux/man-pages/man3/getaddrinfo.3.html#DESCRIPTION
    -- \* POSIX, BSD: `[]`.
    --   * https://man.freebsd.org/cgi/man.cgi?query=getaddrinfo&sektion=3
    --
    -- So, specifying `AI_ADDRCONFIG` explicitly.
    hint =
        S.defaultHints
            { addrFlags = [AI_ADDRCONFIG]
            , addrSocketType = Datagram
            }

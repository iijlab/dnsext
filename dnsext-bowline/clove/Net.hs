module Net where

import qualified Control.Exception as E
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.IP (IP)
import qualified Data.List.NonEmpty as NE
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Timeout (timeout)

serverSocket :: PortNumber -> HostName -> IO Socket
serverSocket pn addr = serverResolve pn addr >>= openSock

serverResolve :: PortNumber -> HostName -> IO AddrInfo
serverResolve pn addr = NE.head <$> getAddrInfo (Just hints) (Just addr) (Just port)
  where
    port = show pn
    hints =
        defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE]
            , addrSocketType = Datagram
            }

-- | A socket to answer on, for one address of the configuration.
--
--   An address of one family serves that family and nothing else.  Left
--   alone, a socket bound to @::@ would take IPv4 peers as well on most
--   systems -- but not on all of them, and not over TCP, where
--   network-run sets this for us: the same configuration answered a v4
--   query over UDP and refused the same query over TCP, which is no way
--   to run a server.  Two families means two addresses in the
--   configuration, and it means the same thing for both transports.
openSock :: AddrInfo -> IO Socket
openSock ai = E.bracketOnError (openSocket ai) close $ \s -> do
    setSocketOption s ReuseAddr 1
    when (addrFamily ai == AF_INET6) $ setSocketOption s IPv6Only 1
    bind s $ addrAddress ai
    return s

----------------------------------------------------------------

-- | Asking one question over UDP and waiting for an answer we will
--   take, a few times over.
--
--   One socket for the whole of it, as a resolver does.  An answer to
--   the first asking is still the answer when it arrives during the
--   second, and a socket opened afresh for each try cannot hear it.
--
--   Whatever arrives and is not the answer -- somebody else's, a stale
--   one, or somebody's guess at what we asked -- is passed over and the
--   wait goes on, so that one datagram sent by anyone who can beat the
--   far end to it cannot stand in for the answer.  RFC 8945 Sec 5.4
--   asks for exactly that of a message whose TSIG does not check out:
--   log it and go on waiting.  Why the last one would not do is what
--   comes back when the time runs out.
askUDP
    :: Int
    -- ^ how many times to ask before giving up
    -> Int
    -- ^ how long to wait for an answer to each asking, in microseconds
    -> IP
    -> PortNumber
    -> BS.ByteString
    -- ^ the question, encoded
    -> (BS.ByteString -> Either String a)
    -- ^ what to make of an answer, or why it is not one
    -> IO (Either String a)
askUDP tries tmo ip port out take_ = E.bracket (openUDP ip port) close ask
  where
    ask sock = do
        why <- newIORef "no answer"
        let waiting = do
                bs <- NSB.recv sock 2048
                case take_ bs of
                    Right a -> return a
                    Left w -> writeIORef why w >> waiting
            go n
                | n <= 0 = Left <$> readIORef why
                | otherwise = do
                    _ <- NSB.send sock out
                    ma <- timeout tmo waiting
                    maybe (go (n - 1)) (return . Right) ma
        go tries

-- | A connected datagram socket, so that only the one we asked can be
--   heard from.
openUDP :: IP -> PortNumber -> IO Socket
openUDP ip port = do
    ai <- NE.head <$> getAddrInfo (Just hints) (Just $ show ip) (Just $ show port)
    E.bracketOnError (openSocket ai) close $ \sock -> do
        connect sock $ addrAddress ai
        return sock
  where
    hints =
        defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
            , addrSocketType = Datagram
            }

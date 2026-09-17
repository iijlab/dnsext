module Net where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IP
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

openSock :: AddrInfo -> IO Socket
openSock ai = E.bracketOnError (openSocket ai) close $ \s -> do
    setSocketOption s ReuseAddr 1
    bind s $ addrAddress ai
    return s

----------------------------------------------------------------

-- | Asking one question over UDP and waiting for the answer, a few
--   times over.  A socket of our own rather than the resolver's: a
--   message which has to carry a TSIG has to be ours to build, and the
--   answer to it ours to check against what we sent.
askUDP
    :: Int
    -- ^ how many times to ask before giving up
    -> Int
    -- ^ how long to wait for each answer, in microseconds
    -> IP
    -> PortNumber
    -> BS.ByteString
    -- ^ the question, encoded
    -> IO (Maybe BS.ByteString)
askUDP tries tmo ip port out = go tries
  where
    go n
        | n <= 0 = return Nothing
        | otherwise = do
            manswer <- once
            case manswer of
                Just bs -> return $ Just bs
                Nothing -> go (n - 1)
    once = E.bracket (openUDP ip port) close $ \sock -> do
        _ <- NSB.send sock out
        timeout tmo $ NSB.recv sock 2048

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

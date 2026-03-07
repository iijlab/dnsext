module Net where

import qualified Control.Exception as E
import qualified Data.List.NonEmpty as NE
import Network.Socket

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

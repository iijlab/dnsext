module Net where

import qualified Control.Exception as E
import Control.Monad (when)
import DNS.Types.Decode (decodeVCLength)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.IP (
    AddrRange,
    IP (..),
    IPv4,
    IPv6,
    addrRangePair,
    fromIPv6w,
    makeAddrRange,
    toIPv4w,
 )
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

-- | Largest message a length prefix can announce, which is the most a
--   reader can be asked for whatever limit it is given.
vcLimitMax :: Int
vcLimitMax = 65535

-- | Reading one length-prefixed message from a connection, keeping
--   whatever was read past it for the next one.
--
--   That leftover is the point.  A peer is allowed to send its next
--   query without waiting for the answer to this one (RFC 7766 Sec
--   6.2.1.1 asks clients to), so a read can bring back the end of one
--   message and the beginning of another.  A reader which is made
--   afresh for each message drops what it read ahead, and those
--   queries are never answered and never reported.
recvMessage :: Int -> Socket -> ByteString -> IO (ByteString, ByteString)
recvMessage lim sock rest0 = do
    (lenbs, rest1) <- recvExactly 2 sock rest0
    let len = decodeVCLength lenbs
    when (len > lim) $
        E.throwIO $
            userError $
                "message of " ++ show len ++ " bytes is over the limit of " ++ show lim
    recvExactly len sock rest1

recvExactly :: Int -> Socket -> ByteString -> IO (ByteString, ByteString)
recvExactly n sock rest0 = go [rest0] (BS.length rest0)
  where
    go acc len
        | len >= n = return $ BS.splitAt n $ BS.concat $ reverse acc
        | otherwise = do
            bs <- NSB.recv sock $ max 2048 (n - len)
            if BS.null bs
                then E.throwIO $ userError $ ended len
                else go (bs : acc) (len + BS.length bs)
    -- A peer which has finished closes between messages, which is not
    -- the same thing as one which stops in the middle of one.
    ended 0 = "the peer closed the connection"
    ended _ = "the connection closed in mid message"

-- | The address a peer really is.  What a socket reports for an IPv4
--   peer is an IPv4-mapped IPv6 address -- @::ffff:192.0.2.1@ -- if it
--   ever reports one at all, and that peer is the IPv4 peer
--   @192.0.2.1@: the two spellings name one host.  'Eq' on 'IP' says so
--   already, which is why a list of addresses compares right; a route
--   table cannot, since it is asked for one family or the other, and
--   asking the wrong one is a silent miss.
--
--   As things stand nothing reaches us that way: 'openSock' sets
--   IPV6_V6ONLY and network-run sets it for TCP, so both transports are
--   of one family, and this changes nothing.  It is here for the day
--   one of them is dual stack again, because of what the miss looks
--   like when it happens -- a peer the configuration allows is refused,
--   and nothing anywhere says why.
unmap :: IP -> IP
unmap (IPv6 ip6)
    | (0, 0, 0xffff, w) <- fromIPv6w ip6 = IPv4 $ toIPv4w w
unmap ip = ip

-- | The same for a range from the configuration: @::ffff:192.0.2.0/120@
--   is @192.0.2.0/24@ written the long way.  'Nothing' for a range
--   which is not inside the mapped block, and for one wider than it,
--   which covers addresses that are not mapped anything.
unmapRange :: AddrRange IPv6 -> Maybe (AddrRange IPv4)
unmapRange r
    | (a6, len) <- addrRangePair r
    , len >= 96
    , (0, 0, 0xffff, w) <- fromIPv6w a6 =
        Just $ makeAddrRange (toIPv4w w) (len - 96)
    | otherwise = Nothing

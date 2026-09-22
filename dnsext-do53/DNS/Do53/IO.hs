{-# LANGUAGE OverloadedStrings #-}

module DNS.Do53.IO (
    openTCP,

    -- * Receiving DNS messages
    recvTCP,
    recvVC,
    makeRecvVC,

    -- * Sending pre-encoded messages
    sendTCP,
    sendVC,

    -- * Misc
    makeAddrInfo,
)
where

import qualified Control.Exception as E
import DNS.Do53.Imports
import DNS.Do53.Types
import DNS.Types hiding (Seconds)
import DNS.Types.Decode (decodeVCLength)
import DNS.Types.Encode (encodeVCLength)
import qualified Data.ByteString as BS
import Network.Socket (
    AddrInfo (..),
    Family (..),
    SocketType (..),
    connect,
    defaultProtocol,
    openSocket,
 )
import Network.Socket.BufferPool (makeRecvN)
import Network.Socket.ByteString (recv)
import qualified Network.Socket.ByteString as NSB

----------------------------------------------------------------

-- | Opening a TCP socket.
openTCP :: IP -> PortNumber -> IO Socket
openTCP a p = do
    let ai = makeAddrInfo a p
    sock <- openSocket ai
    connect sock $ addrAddress ai
    return sock

makeAddrInfo :: IP -> PortNumber -> AddrInfo
makeAddrInfo a p =
    AddrInfo
        { addrFlags = []
        , addrFamily = case a of
            IPv4 _ -> AF_INET
            IPv6 _ -> AF_INET6
        , addrSocketType = Stream
        , addrProtocol = defaultProtocol
        , addrAddress = toSockAddr (a, p)
        , addrCanonName = Nothing
        }

----------------------------------------------------------------

-- TCP and QUIC has its own RecvN (i.e., Int -> IO BS).
-- TLS has Recv. This must be converted to RecvN by makeRecvN in the
-- "recv" package. If not converted, a message is also read when
-- obtaining the length of the message!

-- | Receiving data from a virtual circuit.
-- This function returns exactly-necessary-length data.
-- If necessary-length data is not received, an exception is thrown.
--
-- A reader is made for the one message and thrown away with it, so
-- whatever the socket handed over beyond that message goes too.  That
-- is only safe where nothing else is coming: for a connection which
-- carries more than one message, use 'makeRecvVC' and keep the reader
-- it gives.
recvVC :: VCLimit -> IO BS -> IO BS
recvVC lim rcv = do
    recvN <- makeRecvN "" rcv
    recvVCwith lim recvN

-- | A reader for a virtual circuit, made once for the connection.
--
-- What a read brings back beyond the message being asked for belongs to
-- the next one, and the reader is what holds it.  Made afresh for each
-- message, as 'recvVC' does, it takes those octets with it when it
-- goes: a peer which sends its next answer without waiting -- which is
-- what answering a pipelined query looks like -- has everything after
-- the first message in a read dropped.
makeRecvVC :: VCLimit -> IO BS -> IO (IO BS)
makeRecvVC lim rcv = recvVCwith lim <$> makeRecvN "" rcv

recvVCwith :: VCLimit -> (Int -> IO BS) -> IO BS
recvVCwith lim recvN = do
    b2 <- recvN 2
    let len = decodeVCLength b2
    when (fromIntegral len > lim) $
        E.throwIO $
            DecodeError $
                "length is over the limit: should be len <= lim, but (len: "
                    ++ show len
                    ++ ") > (lim: "
                    ++ show lim
                    ++ ") "
    bs <- recvN len
    whole len bs
  where
    {- The whole of it, or none of it.  A read gives back what has
       arrived, and a peer which sent the length and then went away
       leaves some of the message behind: that is the beginning of a
       message and not a short one, and the rest of it is never coming.
       Handing it on only moves the failure to whoever decodes it, who
       is in no position to say what went wrong and, on a connection
       which is read in a loop, says it again for every message after
       this one.

       Nothing at all is the same thing said by a peer which went away
       sooner, and includes the case of a length which did not arrive
       either: `decodeVCLength` calls a prefix it could not read zero
       octets long. -}
    whole len bs
        | BS.null bs = E.throwIO $ DecodeError "message length is not enough"
        | BS.length bs /= len =
            E.throwIO $
                DecodeError $
                    "message stopped short: should be "
                        ++ show len
                        ++ " octets, but "
                        ++ show (BS.length bs)
                        ++ " arrived"
        | otherwise = return bs

-- | Receiving data from a TCP socket.
recvTCP :: Socket -> IO BS
recvTCP sock = recv sock 2048

----------------------------------------------------------------

-- | Send a single encoded 'DNSMessage' over VC.  An explicit length is
-- prepended to the encoded buffer before transmission.  If you want to
-- send a batch of multiple encoded messages back-to-back over a single
-- VC connection, and then loop to collect the results, use 'encodeVC'
-- to prefix each message with a length, and then use 'sendAll' to send
-- a concatenated batch of the resulting encapsulated messages.
sendVC :: ([BS] -> IO ()) -> BS -> IO ()
sendVC writev bs = do
    let lb = encodeVCLength $ BS.length bs
    writev [lb, bs]

-- | Sending data to a TCP socket.
sendTCP :: Socket -> [BS] -> IO ()
sendTCP = NSB.sendMany

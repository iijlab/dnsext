{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Two DNS servers on the loopback for the tests to talk to: one which
--   answers from a small table, and one which never answers at all.
--
--   The tests used to ask 8.8.8.8 and 1.1.1.1 about real names, so they
--   failed wherever there was no internet, and to ask 192.0.2.1 and
--   192.0.2.2 in the hope of a timeout, so they failed wherever those
--   addresses are not a black hole.  That last one is not hypothetical:
--   on a machine which answers "network is unreachable" for
--   documentation addresses, @resolves well concurrently (2)@ got
--   'NetworkFailure' where it wanted 'RetryLimitExceeded'.
--
--   A socket which is bound and never read from is a black hole which
--   does not depend on anybody's routing table: the datagram is taken
--   by the kernel, nothing comes back, and the resolver times out and
--   retries exactly as it would against a server which has stopped
--   listening to it.
module FakeServer (
    withAnswering,
    withSilent,
    answered,
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IP (IPv4, IPv6)
import Network.Socket
import qualified Network.Socket.ByteString as NSB

import DNS.Do53.Internal (recvTCP, recvVC, sendTCP, sendVC)
import DNS.Types
import DNS.Types.Decode (decode)
import DNS.Types.Encode (encode)

-- | What the fake server knows.  Everything else it answers with
--   NXDOMAIN, and a name it knows of a type it does not with NOERROR
--   and nothing in it -- which is what @lookupAAAA \"ipv4.tlund.se\"@
--   is there to see.
table :: [((Domain, TYPE), [RData])]
table =
    [ (("www.mew.org.", A), [rd_a (read "192.0.2.10" :: IPv4)])
    , (("mew.org.", A), [rd_a (read "192.0.2.11" :: IPv4)])
    , (("mew.org.", MX), [rd_mx 10 "mail.mew.org."])
    , (("mew.org.", TXT), [rd_txt "v=spf1 -all"])
    , (("mew.org.", NS), [rd_ns "ns1.mew.org.", rd_ns "ns2.mew.org."])
    ,
        ( ("mew.org.", SOA)
        , [rd_soa "ns1.mew.org." "hostmaster.mew.org." 1 3600 900 604800 900]
        )
    , (("google.com.", AAAA), [rd_aaaa (read "2001:db8::1" :: IPv6)])
    , (("ipv4.tlund.se.", A), [rd_a (read "192.0.2.12" :: IPv4)])
    ]

-- | Names the table has something of some type for.  A query for a
--   name which is not one of these is a name error.
known :: [Domain]
known = map (fst . fst) table

-- | The answer to a query, as the fake server sees it.
answered :: DNSMessage -> DNSMessage
answered query = reply{answer = rrs, rcode = rc}
  where
    Question{..} = question query
    rrs =
        [ ResourceRecord qname qtype qclass 3600 rd
        | Just rds <- [lookup (qname, qtype) table]
        , rd <- rds
        ]
    rc
        | not (null rrs) = NoErr
        | qname `elem` known = NoErr
        | otherwise = NXDomain
    reply =
        query
            { flags = (flags query){isResponse = True, recAvailable = True}
            , ednsHeader = case ednsHeader query of
                EDNSheader _ -> EDNSheader defaultEDNS
                h -> h
            , answer = []
            , authority = []
            , additional = []
            }

-- | A server on the loopback, over UDP and TCP on one port, for as long
--   as the body runs.
withAnswering :: (PortNumber -> IO a) -> IO a
withAnswering = withServer serveUDP serveTCP

-- | A server which takes a query and says nothing.  The sockets are
--   bound, which is the whole of it: nothing ever reads them.
withSilent :: (PortNumber -> IO a) -> IO a
withSilent = withServer idle idle
  where
    idle _ = forever $ threadDelay 1000000

withServer :: (Socket -> IO ()) -> (Socket -> IO ()) -> (PortNumber -> IO a) -> IO a
withServer udp tcp body =
    E.bracket openBoth closeBoth $ \(us, ts, port) ->
        E.bracket (forkIO $ udp us) killThread $ \_ ->
            E.bracket (forkIO $ tcp ts) killThread $ \_ ->
                body port
  where
    closeBoth (us, ts, _) = close us >> close ts

-- | One port for both transports: the datagram socket picks it and the
--   stream socket is bound to the same one.
--
--   A number the kernel has free for a datagram socket says nothing
--   about whether it is free for a stream socket, so the second bind
--   can fail while the first has just succeeded -- and does, on a
--   machine busy enough, as a test which has nothing to do with sockets
--   failing with "Address already in use".  There is no way to ask for
--   a number in both at once, so the answer is to ask again.
openBoth :: IO (Socket, Socket, PortNumber)
openBoth = go (20 :: Int)
  where
    go 0 = fail "FakeServer: no port free for both a datagram and a stream socket"
    go n = do
        us <- socket AF_INET Datagram defaultProtocol
        bind us $ SockAddrInet 0 localhost
        SockAddrInet port _ <- getSocketName us
        ts <- socket AF_INET Stream defaultProtocol
        setSocketOption ts ReuseAddr 1
        taken <- E.try $ bind ts (SockAddrInet port localhost) >> listen ts 10
        case taken of
            Right () -> return (us, ts, port)
            Left e -> do
                close us
                close ts
                const (go (n - 1)) (e :: E.IOException)
    localhost = tupleToHostAddress (127, 0, 0, 1)

serveUDP :: Socket -> IO ()
serveUDP s = forever $ do
    (bs, peer) <- NSB.recvFrom s 2048
    mapM_ (\out -> void $ NSB.sendTo s out peer) $ replyTo bs

serveTCP :: Socket -> IO ()
serveTCP s = forever $ do
    (conn, _) <- accept s
    -- A peer which has finished with the connection closes it, and the
    -- read of the next length prefix throws.  That is the end of this
    -- connection and nothing else; without catching it the thread dies
    -- with an exception nobody is waiting for, which takes the test
    -- process with it.
    void $ forkIO $ (`E.finally` close conn) $ ignoring $ forever $ do
        bs <- recvVC (8 * 1024) $ recvTCP conn
        mapM_ (sendVC $ sendTCP conn) $ replyTo bs

ignoring :: IO () -> IO ()
ignoring action = action `E.catch` \e -> const (return ()) (e :: E.SomeException)

replyTo :: ByteString -> Maybe ByteString
replyTo bs = case decode bs of
    Left _ -> Nothing
    Right query
        | BS.null bs -> Nothing
        | otherwise -> Just $ encode $ answered query

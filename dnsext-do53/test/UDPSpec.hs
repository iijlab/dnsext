{-# LANGUAGE OverloadedStrings #-}

module UDPSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Types
import DNS.Types.Decode (decode)
import DNS.Types.Encode (encode)
import qualified DNS.Types.Opaque as Opaque
import qualified Data.ByteString as BS
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import Test.Hspec

spec :: Spec
spec = describe "a UDP answer" $ do
    -- The receive buffer was 2048 octets whatever the query had said it
    -- could take, so an answer to a query which asked for more was cut
    -- short by the kernel and thrown out as undecodable.
    it "has room for as much as the query asked for" $
        withServerOf 12 $ \port -> do
            r <- udpResolver (resolveInfo port) theQuestion $ ednsSetUdpSize (Just 4096)
            case r of
                Right rply -> replyRxBytes rply `shouldSatisfy` (> 2048)
                Left e -> expectationFailure $ show e

    it "is taken when it fits in what the query asked for" $
        withServerOf 2 $ \port -> do
            r <- udpResolver (resolveInfo port) theQuestion mempty
            case r of
                Right rply -> replyRxBytes rply `shouldSatisfy` (< 2048)
                Left e -> expectationFailure $ show e

theQuestion :: Question
theQuestion = Question "big.example" TXT IN

resolveInfo :: PortNumber -> ResolveInfo
resolveInfo port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 1000000}
        }

-- | A server which answers everything with a TXT record of the given
--   number of character-strings, 255 octets each.
withServerOf :: Int -> (PortNumber -> IO a) -> IO a
withServerOf strings body = E.bracket open close $ \sock -> do
    port <- socketPort sock
    E.bracket (forkIO $ serve sock) killThread $ \_ -> body port
  where
    open = do
        sock <- socket AF_INET Datagram defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        return sock
    serve sock = forever $ do
        (wire, peer) <- NSB.recvFrom sock 4096
        case decode wire of
            Left _ -> return ()
            Right qry -> void $ NSB.sendTo sock (encode $ reply qry) peer
    reply qry =
        qry
            { flags = (flags qry){isResponse = True}
            , answer = [txt $ qname $ question qry]
            }
    txt name =
        ResourceRecord
            { rrname = name
            , rrtype = TXT
            , rrclass = IN
            , rrttl = 60
            , rdata = rd_txt_n $ replicate strings $ Opaque.fromByteString $ BS.replicate 255 0x78
            }

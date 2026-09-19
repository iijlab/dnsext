{-# LANGUAGE OverloadedStrings #-}

module VCSpec (spec) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.IP (IPv4)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import Test.Hspec

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Types
import DNS.Types.Decode (decode, decodeVCLength)
import DNS.Types.Encode (encode, encodeVCLength)

spec :: Spec
spec = describe "a persistent connection" $ do
    -- A client which pipelines -- which RFC 7766 Sec 6.2.1.1 asks it to
    -- -- gets its answers back to back, and more than one of them lands
    -- in a single read.  Everything after the first in a read used to be
    -- dropped, and those queries waited out their timeout.
    it "takes every answer which arrives in one read" $
        withAnswering 3 $ \port -> do
            rs <- resolveAll port [name i | i <- [1 .. 3]]
            map (fmap replied) rs `shouldBe` map (Right . Just) [name i | i <- [1 .. 3]]

    it "takes them one at a time as well" $
        withAnswering 1 $ \port -> do
            rs <- resolveAll port [name i | i <- [1 .. 3]]
            map (fmap replied) rs `shouldBe` map (Right . Just) [name i | i <- [1 .. 3]]

name :: Int -> Domain
name i = fromRepresentation $ "h" ++ show i ++ ".example."

replied :: Reply -> Maybe Domain
replied r = case answer (replyDNSMessage r) of
    rr : _ -> Just (rrname rr)
    [] -> Nothing

resolveInfo :: PortNumber -> ResolveInfo
resolveInfo port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoVCLimit = 8 * 1024
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 3000000}
        }

resolveAll :: PortNumber -> [Domain] -> IO [Either DNSError Reply]
resolveAll port ns = do
    out <- newIORef []
    tcpPersistentResolver (resolveInfo port) $ \ask ->
        writeIORef out =<< mapConcurrently (\n -> ask (Question n A IN) mempty) ns
    readIORef out

----------------------------------------------------------------

-- | A server on the loopback which answers questions it is asked,
--   holding them back until it has the given number and then writing
--   all the answers at once.
withAnswering :: Int -> (PortNumber -> IO a) -> IO a
withAnswering batch = withServer $ \conn ->
    let go rest held
            | length held == batch = do
                NSB.sendAll conn $ BS.concat [encodeVCLength (BS.length b) <> b | b <- reverse held]
                go rest []
            | otherwise = do
                (bs, rest') <- oneMessage conn rest
                go rest' (answerTo bs : held)
     in go BS.empty []

answerTo :: ByteString -> ByteString
answerTo bs = case decode bs of
    Left e -> error $ "the fake server was sent something it could not read: " ++ show e
    Right q ->
        encode
            q
                { flags = (flags q){isResponse = True, recAvailable = True}
                , answer = [ResourceRecord (qname (question q)) A IN 60 (rd_a (read "10.0.0.1" :: IPv4))]
                }

-- | One length-prefixed message, and whatever was read past it.
oneMessage :: Socket -> ByteString -> IO (ByteString, ByteString)
oneMessage conn rest0 = do
    (lenbs, rest1) <- exactly 2 rest0
    exactly (decodeVCLength lenbs) rest1
  where
    exactly n rest
        | BS.length rest >= n = pure $ BS.splitAt n rest
        | otherwise = do
            bs <- NSB.recv conn 2048
            if BS.null bs then E.throwIO (userError "the peer closed") else exactly n (rest <> bs)

withServer :: (Socket -> IO ()) -> (PortNumber -> IO a) -> IO a
withServer serve body = E.bracket open close $ \sock -> do
    SockAddrInet port _ <- getSocketName sock
    E.bracket (forkIO $ accepting sock) killThread $ \_ -> body port
  where
    open = do
        s <- socket AF_INET Stream defaultProtocol
        setSocketOption s ReuseAddr 1
        bind s $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen s 10
        pure s
    accepting sock = forever $ do
        (conn, _) <- accept sock
        -- A peer which has finished closes, and the next read throws;
        -- that is the end of the connection and nothing more.
        void $ forkIO $ (`E.finally` close conn) $ ignoring $ serve conn
    ignoring act = act `E.catch` \e -> const (pure ()) (e :: E.SomeException)

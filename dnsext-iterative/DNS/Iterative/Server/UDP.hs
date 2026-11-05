{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.UDP (
    udpServers,
    UdpServerConfig (..),
    setPktInfo,
)
where

-- GHC packages
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (void, when)
import Data.IORef
import qualified Data.IntSet as Set

-- dnsext-utils
import qualified DNS.ThreadAsync as TAsync
import DNS.Transport.Types

-- other packages
import Network.Socket (
    SocketOption (..),
    getSocketName,
    setSocketOption,
 )
import qualified Network.Socket.ByteString as NSB

-- this package
import DNS.Iterative.Internal (DNSQuery, FoldResponse, Env (..))
import DNS.Iterative.Server.Pipeline
import DNS.Iterative.Server.Types
import DNS.Iterative.Stats (incStatsUDP53)

----------------------------------------------------------------

data UdpServerConfig = UdpServerConfig
    { udp_interface_automatic :: Bool
    , udp_stop :: STM ()
    -- ^ Fires when the server is told to stop.
    , udp_drain :: IO Bool
    -- ^ Whether the datagrams already come are still read once stopped:
    --   when nothing takes the socket over.
    }

----------------------------------------------------------------

udpServers :: UdpServerConfig -> ServerActions
udpServers conf synth env toCacher ss = do
    concat <$> mapM (udpServer conf env synth foldCached foldIterative toCacher) ss
  where
    (_synth, foldCached, foldIterative) = extendSynthesis synth

udpServer
    :: UdpServerConfig -> Env -> Synthesis
    -> (forall p . DNSQuery p -> FoldResponse IO p)
    -> (forall q . FoldResponse IO q)
    -> (ToCacher -> IO ()) -> Socket -> IO [IO ()]
udpServer UdpServerConfig{..} env synth foldCached foldIterative toCacher s = do
    mysa <- getSocketName s
    when udp_interface_automatic $ setPktInfo s
    {- limit waiting area on server to constant size -}
    let queueBound = 64
    qs <- newTBQueueIO queueBound
    -- Once stopped, the sender goes on until every query read is answered
    -- or dropped.
    pendings <- newTVarIO Set.empty
    counter <- newIORef 0
    -- Once draining, what was read in its window and not yet handed on.
    draining <- newIORef Nothing
    recvDone <- newTVarIO False
    let newPending = do
            i <- atomicModifyIORef' counter $ \n -> (n + 1, n)
            pendingOp pendings i $ \add pop -> pop <$ add
        answered = readTVar pendings >>= check . Set.null
        toSender = atomically . writeTBQueue qs
        -- After the receiver, which may still be reading once stopped.
        fromX = atomically $ (Just <$> readTBQueue qs) `orElse` (Nothing <$ (readTVar recvDone >>= check >> answered))
        recvOr = NSB.recvMsgWith s 2048 2048 0
        recv = do
            d <- readIORef draining
            case d of
                Just xs -> case xs of
                    [] -> return Nothing
                    x : rest -> writeIORef draining (Just rest) >> received x
                Nothing -> do
                    r <- recvOr udp_stop
                    case r of
                        Right x -> received x
                        Left () -> do
                            drain <- udp_drain
                            if drain
                                then (drained >>= writeIORef draining . Just) >> recv
                                else return Nothing
        -- What has come, read at once rather than as the pipeline takes it:
        -- until a read would wait, within a window, so that what comes under
        -- a steady flow is not taken for what had come.
        drained = do
            window <- registerDelay drainWindow
            let loop acc = do
                    grace <- registerDelay 1000
                    r <- recvOr $ (readTVar grace >>= check) `orElse` (readTVar window >>= check)
                    either (const $ return $ reverse acc) (loop . (: acc)) r
            loop []
        received (peersa, bs, cmsgs, _) = do
            incStatsUDP53 peersa (stats_ env)
            return $ Just (bs, PeerInfoUDP peersa cmsgs)
        send bs (PeerInfoUDP peersa cmsgs) =
            void $ NSB.sendMsg s peersa [bs] cmsgs 0
        send _ _ = return ()
        receiver = receiverLogic env mysa recv newPending toCacher toSender UDP synth foldCached foldIterative
        sender = senderLogic env send fromX
        receiver' = receiver `finally` atomically (writeTVar recvDone True)
    return [TAsync.concurrently_ "bw.udp-send" sender "bw.udp-recv" receiver']

-- | How long a quitting server goes on reading what has come (microseconds).
drainWindow :: Int
drainWindow = 100000

setPktInfo :: Socket -> IO ()
setPktInfo s = do
    sa <- getSocketName s
    setSocketOption s (decideOption sa) 1

decideOption :: SockAddr -> SocketOption
decideOption SockAddrInet{} = RecvIPv4PktInfo
decideOption SockAddrInet6{} = RecvIPv6PktInfo
decideOption _ = error "decideOption"

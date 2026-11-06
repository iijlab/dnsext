{-# LANGUAGE RankNTypes #-}

module DNS.Iterative.Server.Types (
    ServerActions,
    Env,
    VcServerConfig (..),
    ToCacher,
    FromReceiver,
    ToWorker,
    FromCacher,
    ToSender,
    FromX,
    ReqNum,
    VcPendingOp (..),
    ExSynthesis,
    Input (..),
    Output (..),
    Peer (..),
    EpochTimeUsec,
    peerSockAddr,
    withLocationIOE,
    Socket,
    SockAddr (..),
    withFdSocket,
    socketName,
    SuperStream (..),
) where

-- GHC
import Data.ByteString (ByteString)
import System.IO.Error (ioeSetLocation, tryIOError)

-- dnsext-types
import DNS.Types (DNSMessage)
import DNS.Types.Time (EpochTimeUsec)
import Data.IP (fromSockAddr)

-- dnsext-utils
import DNS.Transport.Types (DoX (..), Synthesis (..))

-- other packages
import qualified Network.HTTP2.Server.Internal as H2I
import qualified Network.QUIC as QUIC
import Network.Socket
import Network.TLS (Credentials (..), SessionManager)

-- this package
import DNS.Iterative.Query (DNSQuery, Env, FoldResponse)
import DNS.Iterative.Server.Stop (Stopper)

data SuperStream = StreamH2 H2I.Stream | StreamQUIC QUIC.Stream deriving (Show)

data Peer
    = PeerInfoUDP SockAddr [Cmsg]
    | PeerInfoStream SockAddr SuperStream
    | PeerInfoVC SockAddr
    deriving (Show)

peerSockAddr :: Peer -> SockAddr
peerSockAddr (PeerInfoUDP sa _) = sa
peerSockAddr (PeerInfoStream sa _) = sa
peerSockAddr (PeerInfoVC sa) = sa

-- request identifier in one connection
type ReqNum = Int

data VcPendingOp
    = VcPendingOp
    { vpReqNum :: ReqNum
    , vpDelete :: IO ()
    }

type ExSynthesis p q = (Synthesis, DNSQuery p -> FoldResponse IO p, FoldResponse IO q)

data Input a = Input
    { inputQuery :: a
    , inputPendingOp :: VcPendingOp
    , inputMysa :: SockAddr
    , inputPeerInfo :: Peer
    , inputDoX :: DoX
    , inputSynthesis :: Synthesis
    , inputFoldCached :: forall b . DNSQuery b -> FoldResponse IO b
    , inputFoldIterative :: forall b . FoldResponse IO b
    , inputToSender :: ToSender -> IO ()
    , inputRecvTime :: EpochTimeUsec
    }

data Output = Output
    { outputReplyBS :: ByteString
    , outputPendingOp :: VcPendingOp
    , outputPeerInfo :: Peer
    }

-- Type of the action reveals its arguments and the context of the monad,
--   eg.
--   - toCacher :: ToCacher -> IO ()
--   - toSender :: IO ToSender
type ToCacher = Input ByteString
type FromReceiver = Input ByteString
type ToWorker = Input DNSMessage
type FromCacher = Input DNSMessage
type ToSender = Output
type FromX = Output

type ServerActions = Synthesis -> Env -> (ToCacher -> IO ()) -> [Socket] -> IO [IO ()]

data VcServerConfig = VcServerConfig
    { vc_query_max_size :: Int
    , vc_idle_timeout :: Int
    , vc_slowloris_size :: Int
    , vc_credentials :: Credentials
    , vc_session_manager :: SessionManager
    , vc_early_data_size :: Int
    , vc_interface_automatic :: Bool
    , vc_stop :: Stopper
    -- ^ What tells the server to stop.
    , vc_drain :: [Socket] -> IO Bool
    -- ^ Whether the sessions of a server sharing its sockets (QUIC) answer
    --   what they have taken once stopped: when nothing takes them over.
    }

withLocationIOE :: String -> IO a -> IO a
withLocationIOE loc action = do
    either left pure =<< tryIOError action
  where
    left ioe = ioError $ ioeSetLocation ioe loc

socketName :: Socket -> IO String
socketName s = do
    sa <- getSocketName s
    return $ case fromSockAddr sa of
        Nothing -> "(no name)"
        Just (ip, pn) -> show ip ++ "#" ++ show pn

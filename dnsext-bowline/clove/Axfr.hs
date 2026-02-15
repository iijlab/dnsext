module Axfr (
    axfrResponder,
) where

import DNS.Auth.Algorithm
import DNS.Do53.Internal
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode

import Data.IORef
import Data.IP
import Data.IP.RouteTable
import qualified Data.IP.RouteTable as T
import Data.Maybe
import Network.Socket

axfrResponder
    :: IORef DB
    -> IPRTable IPv4 Bool
    -> IPRTable IPv6 Bool
    -> Socket
    -> IO ()
axfrResponder dbref t4 t6 sock = do
    sa <- getSocketName sock
    let ok = case fromSockAddr sa of
            Just (IPv4 ip4, _) -> fromMaybe False $ T.lookup (makeAddrRange ip4 32) t4
            Just (IPv6 ip6, _) -> fromMaybe False $ T.lookup (makeAddrRange ip6 128) t6
            _ -> False
    equery <- decode <$> recvVC (32 * 1024) (recvTCP sock)
    case equery of
        Left _ -> return ()
        Right query
            | ok -> do
                db <- readIORef dbref
                let reply = makeReply db query
                sendVC (sendTCP sock) $ encode reply
            | otherwise -> do
                let reply = (fromQuery query){rcode = Refused}
                sendVC (sendTCP sock) $ encode reply

makeReply :: DB -> DNSMessage -> DNSMessage
makeReply db query
    | qtype (question query) == AXFR = (fromQuery query){answer = dbAll db}
    | otherwise = getAnswer db query

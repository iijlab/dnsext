{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as E
import Control.Monad
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import qualified DNS.ZoneFile as ZF
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Environment

main :: IO ()
main = do
    [zonefile] <- getArgs
    rrs <- catMaybes . map fromResource <$> ZF.parseFile zonefile
    let kvs = map (\r -> ((rrname r, rrtype r), r)) rrs
        m = M.fromList kvs
    s <- serverResolve "127.0.0.1" "53" >>= serverSocket
    clove s m

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

----------------------------------------------------------------

serverResolve :: HostName -> ServiceName -> IO AddrInfo
serverResolve addr port = NE.head <$> getAddrInfo (Just hints) (Just addr) (Just port)
  where
    hints =
        defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV, AI_PASSIVE]
            , addrSocketType = Datagram
            }

serverSocket :: AddrInfo -> IO Socket
serverSocket ai = E.bracketOnError (openSocket ai) close $ \s -> do
    setSocketOption s ReuseAddr 1
    bind s $ addrAddress ai
    return s

clove :: Socket -> M.Map (Domain, TYPE) ResourceRecord -> IO ()
clove s m = loop
  where
    loop = do
        (bs, sa) <- NSB.recvFrom s 2048
        case decode bs of
            Left _e -> return ()
            Right query -> do
                let [q] = question query
                    as = case M.lookup (qname q, qtype q) m of
                        Nothing -> []
                        Just a -> [a]
                    bs' = encode $ query{answer = as}
                void $ NSB.sendTo s bs' sa
        loop

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Notify (notify) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IP
import qualified Data.List.NonEmpty as NE
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import qualified System.IO.Error as E
import System.Posix.Time (epochTime)
import System.Timeout (timeout)

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.TSIG
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import DNS.Types.Time (EpochTime)

import Exception
import Types

----------------------------------------------------------------

-- | How long to wait for a secondary to say it heard.
notifyTimeout :: Int
notifyTimeout = 3 * 1000000

-- | How many times to say it.  RFC 1996 Sec 3.6 wants a notify
--   repeated until it is acknowledged; this gives up sooner, since the
--   secondary asks for the SOA on its own schedule anyway.
notifyTries :: Int
notifyTries = 3

----------------------------------------------------------------

-- | Telling a secondary that the zone has moved on.
notify :: Env -> Maybe TSIGKey -> Domain -> IP -> PortNumber -> IO (Maybe DNSMessage)
notify Env{..} mkey dom ip port = withNotified $ do
    now <- currentTime
    (out, mrequestMAC) <- asked now
    manswer <- attempt notifyTries out
    case manswer of
        Nothing -> unanswered "no answer"
        Just bs -> case decode bs of
            Left e -> unanswered $ show e
            Right msg -> checked now mrequestMAC bs msg
  where
    q = Question dom SOA IN
    -- RFC 1996: an opcode of its own, and the question is the zone.
    qctl = rdFlag FlagClear <> doFlag FlagClear <> aaFlag FlagSet <> opCode OP_NOTIFY
    peer = "@" ++ show ip ++ "#" ++ show port ++ " \"" ++ toRepresentation dom ++ "\""

    asked now = do
        let bare = encodeQuery 0 q qctl
        case mkey of
            Nothing -> return (bare, Nothing)
            Just key -> case decode bare of
                Left e -> E.ioError $ E.userError $ show e
                Right m -> do
                    let (rr, mac) = signTSIG key now defaultFudge Nothing bare
                    return (encode m{additional = additional m ++ [rr]}, Just mac)

    attempt 0 _ = return Nothing
    attempt n out = do
        r <- exchange ip port out
        case r of
            Just bs -> return $ Just bs
            Nothing -> attempt (n - 1 :: Int) out

    -- The answer is only an acknowledgement, so a bad one is worth
    -- saying out loud and no more: the zone is not riding on it.
    checked now mrequestMAC bs msg = case mkey of
        Nothing -> return $ Just msg
        Just key -> case verifyTSIG (held key) now mrequestMAC bs msg of
            TSIGOk _ -> return $ Just msg
            TSIGMissing -> unanswered "the answer is not signed"
            TSIGFailed fault -> unanswered $ case tsigReported msg of
                -- Sec 5.4: a refusal comes unsigned, so what it says is
                -- worth more than what checking it as an answer makes of
                -- it.
                Just e -> "the far end says " ++ show e
                Nothing -> show fault

    held key n = if n == tsigKeyName key then Just key else Nothing

    unanswered why = do
        envPutLines WARNING Nothing ["    NOTIFY " ++ peer ++ ": " ++ why]
        return Nothing

    withNotified action = do
        er <- trySync action
        case er of
            Right a -> return a
            Left se -> do
                envPutLines WARNING Nothing ["    NOTIFY " ++ peer ++ ": " ++ show se]
                return Nothing

----------------------------------------------------------------

-- | One question and, if it comes, one answer.
exchange :: IP -> PortNumber -> BS.ByteString -> IO (Maybe BS.ByteString)
exchange ip port out = E.bracket (openUDP ip port) close $ \sock -> do
    _ <- NSB.send sock out
    timeout notifyTimeout $ NSB.recv sock 2048

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

currentTime :: IO EpochTime
currentTime = fromIntegral . fromEnum <$> epochTime

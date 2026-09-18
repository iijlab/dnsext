{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Notify (notify) where

import qualified Control.Exception as E
import Data.Functor (($>))
import Data.IP
import Network.Socket (PortNumber)
import qualified System.IO.Error as E
import System.Posix.Time (epochTime)

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.Log
import DNS.TSIG
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import DNS.Types.Time (EpochTime)

import Exception
import Net
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
    ident <- singleGenId
    (out, mrequestMAC) <- asked now ident
    eanswer <- askUDP notifyTries notifyTimeout ip port out $ taken ident now mrequestMAC
    either unanswered (return . Just) eanswer
  where
    q = Question dom SOA IN
    -- RFC 1996: an opcode of its own, and the question is the zone.
    qctl = rdFlag FlagClear <> doFlag FlagClear <> aaFlag FlagSet <> opCode OP_NOTIFY
    peer = "@" ++ show ip ++ "#" ++ show port ++ " \"" ++ toRepresentation dom ++ "\""

    asked now ident = do
        let bare = encodeQuery ident q qctl
        case mkey of
            Nothing -> return (bare, Nothing)
            Just key -> case decode bare of
                Left e -> E.ioError $ E.userError $ show e
                Right m -> do
                    -- Signed over what will be sent rather than over
                    -- what was encoded a moment ago: the two are the
                    -- same, and the MAC covers octets, so it is better
                    -- not to have to say that they are.
                    let body = encode m
                        (rr, mac) = signTSIG key now defaultFudge Nothing body
                    return (encode m{additional = additional m ++ [rr]}, Just mac)

    -- The answer is only an acknowledgement, so a bad one is worth
    -- saying out loud and no more: the zone is not riding on it.
    taken ident now mrequestMAC bs = do
        msg <- either (Left . show) Right $ decode bs
        case checkRespM q ident msg of
            -- Not an answer to what we asked, so the wait goes on.
            Just e -> Left $ show e
            Nothing -> checked now mrequestMAC bs msg $> msg

    checked now mrequestMAC bs msg = case mkey of
        Nothing -> Right ()
        Just key -> case verifyTSIG (held key) now mrequestMAC bs msg of
            TSIGOk _ -> Right ()
            TSIGMissing -> Left "the answer is not signed"
            TSIGFailed fault -> Left $ case tsigReported msg of
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

currentTime :: IO EpochTime
currentTime = fromIntegral . fromEnum <$> epochTime

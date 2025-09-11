{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Recursive (recursiveQuery) where

import Codec.Serialise
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import DNS.Do53.Client (
    LookupConf (..),
    QueryControls,
    ResolveActions (..),
    Seeds (..),
    withLookupConf,
 )
import qualified DNS.Do53.Client as DNS
import DNS.Do53.Internal (
    LookupEnv (..),
    NameTag (..),
    PipelineResolver,
    Reply (..),
    ResolveActions (..),
    ResolveInfo (..),
    defaultResolveActions,
    defaultResolveInfo,
    fromNameTag,
    resolve,
    toNameTag,
 )
import DNS.DoX.Client
import qualified DNS.Log as Log
import DNS.Types (Question (..))
import qualified DNS.Types as DNS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.IP (IP (..))
import qualified Data.List as List
import qualified Network.QUIC.Client as QUIC
import Network.Socket (HostName, PortNumber)
import qualified Network.TLS as TLS
import System.Console.ANSI.Types
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)

import Output
import SocketUtil (checkDisableV6)
import Types

----------------------------------------------------------------

printReplySTM
    :: Bool
    -> (DNS.DNSMessage -> STM ())
    -> Log.PutLines STM
    -> Either DNS.DNSError Reply
    -> STM ()
printReplySTM _ _ putLinesSTM (Left err) = putLinesSTM Log.WARN (Just Red) [show err]
printReplySTM printHeader putLnSTM putLinesSTM (Right r@Reply{..}) = do
    when printHeader $ do
        let h = mkHeader r
        putLinesSTM Log.WARN (Just Green) [h]
    putLnSTM replyDNSMessage

mkHeader :: Reply -> String
mkHeader Reply{..} =
    ";; "
        ++ fromNameTag replyTag
        ++ ", Tx:"
        ++ show replyTxBytes
        ++ "bytes"
        ++ ", Rx:"
        ++ show replyRxBytes
        ++ "bytes"

----------------------------------------------------------------

makeResolveInfo
    :: ResolveActions
    -> [(IP, Maybe HostName, PortNumber)]
    -> [ResolveInfo]
makeResolveInfo ractions aps = mk <$> aps
  where
    mk (ip, msvr, port) =
        defaultResolveInfo
            { rinfoIP = ip
            , rinfoPort = port
            , rinfoUDPRetry = 2
            , rinfoActions = ractions
            , rinfoVCLimit = 8192
            , rinfoServerName = msvr
            }

makeAction
    :: Options
    -> TQueue (NameTag, String)
    -> Log.PutLines STM
    -> IO ResolveActions
makeAction Options{..} tq putLinesSTM = do
    keyloglock <- newMVar ()
    resumplock <- newMVar ()
    ss <- load optResumptionFile
    return
        defaultResolveActions
            { ractionLog = \a b c -> atomically $ putLinesSTM a b c
            , ractionUseEarlyData = opt0RTT
            , ractionKeyLog = case optKeyLogFile of
                Nothing -> TLS.defaultKeyLogger
                Just file -> \msg -> safeAppendFile file keyloglock (C8.pack (msg ++ "\n"))
            , ractionValidate = optValidate
            , ractionOnConnectionInfo = \tag info -> atomically $ writeTQueue tq (tag, info)
            , -- don't load the file here since the file is removed.
              ractionResumptionInfo = \tag -> return $ map snd $ List.filter (\(t, _) -> t == tag) ss
            , ractionOnResumptionInfo = case optResumptionFile of
                Nothing -> \_ _ -> return ()
                Just file -> saveResumption file resumplock tq
            }
  where
    load Nothing = return []
    load (Just file) = do
        exist <- doesFileExist file
        if exist
            then do
                ct <- loadResumption file
                removeFile file
                return ct
            else return []

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
getCustomConf
    :: [(IP, Maybe HostName)]
    -> PortNumber
    -> QueryControls
    -> Options
    -> ResolveActions
    -> IO (LookupConf, [ResolveInfo])
getCustomConf ips port ctl Options{..} ractions
  | null ips = return (conf, [])
  | otherwise = do
        let ahs = if optDisableV6NS then [ip4 | ip4@(IPv4{}, _) <- ips] else ips
            ahps = map (\(x,y) -> (x,y,port)) ahs
            aps = map (\(x,_) -> (x,port)) ahs
            ris = makeResolveInfo ractions ahps
        return (conf{lconfSeeds = SeedsAddrPorts aps}, ris)
  where
    conf =
        DNS.defaultLookupConf
            { lconfUDPRetry = 2
            , lconfQueryControls = ctl
            , lconfConcurrent = True
            , lconfActions = ractions
            }
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

recursiveQuery
    :: [(IP, Maybe HostName)]
    -> PortNumber
    -> (DNS.DNSMessage -> STM ())
    -> Log.PutLines STM
    -> [(Question, QueryControls)]
    -> Options
    -> TQueue (NameTag, String)
    -> IO ()
recursiveQuery ips port putLnSTM putLinesSTM qcs opt@Options{..} tq = do
    ractions <- makeAction opt tq putLinesSTM
    (conf, ris) <- getCustomConf ips port mempty opt ractions
    pipes <-
        if optDoX == "auto"
            then resolveDDR opt conf
            else resolveDoX opt ris
    let printHeader = optFormat `notElem` [Short, JSONstyle]
    if null pipes
        then runUDP printHeader conf putLnSTM putLinesSTM qcs
        else runVC printHeader pipes putLnSTM putLinesSTM qcs

----------------------------------------------------------------

runUDP
    :: Bool
    -> LookupConf
    -> (DNS.DNSMessage -> STM ())
    -> Log.PutLines STM
    -> [(Question, QueryControls)]
    -> IO ()
runUDP printHeader conf putLnSTM putLinesSTM qcs = withLookupConf conf $ \LookupEnv{..} -> do
    let printIt (q, ctl) = resolve lenvResolveEnv q ctl >>= atomically . printReplySTM printHeader putLnSTM putLinesSTM
    mapM_ printIt qcs

runVC
    :: Bool
    -> [PipelineResolver]
    -> (DNS.DNSMessage -> STM ())
    -> Log.PutLines STM
    -> [(Question, QueryControls)]
    -> IO ()
runVC printHeader pipes putLnSTM putLinesSTM qcs = do
    refs <- replicateM len $ newTVarIO False
    let targets = zip qcs refs
    -- raceAny cannot be used to ensure that TLS sessino tickets
    -- are certainly saved.
    rs <- mapConcurrently (E.try . resolver printHeader putLnSTM putLinesSTM targets) pipes
    case foldr1 op rs of
        Right _ -> return ()
        Left e -> do
            print (e :: DNS.DNSError)
            exitFailure
  where
    len = length qcs
    r@(Right _) `op` _ = r
    _ `op` l = l

----------------------------------------------------------------

resolver
    :: Bool
    -> (DNS.DNSMessage -> STM ())
    -> Log.PutLines STM
    -> [((Question, QueryControls), TVar Bool)]
    -> PipelineResolver
    -> IO ()
resolver printHeader putLnSTM putLinesSTM targets pipeline = pipeline $ \resolv -> do
    -- running concurrently for multiple target domains
    rs <- mapConcurrently (printIt resolv) targets
    case foldr op (Right ()) rs of
        Right () -> return ()
        Left e -> E.throwIO (e :: DNS.DNSError)
  where
    l@(Left _) `op` _ = l
    _ `op` r = r
    printIt resolv ((q, ctl), tvar) = E.try $ do
        er <- resolv q ctl
        atomically $ do
            done <- readTVar tvar
            unless done $ do
                printReplySTM printHeader putLnSTM putLinesSTM er
                writeTVar tvar True

----------------------------------------------------------------

resolveDDR
    :: Options
    -> LookupConf
    -> IO [PipelineResolver]
resolveDDR Options{..} conf = do
    er <- withLookupConf conf lookupSVCBInfo
    case er of
        Left err -> do
            print err
            exitFailure
        Right siss0 -> do
            disableV6 <- checkDisableV6 [rinfoIP ri | sis <- siss0, SVCBInfo{..} <- sis, ri <- svcbInfoResolveInfos]
            let isIPv4 (IPv4 _) = True
                isIPv4 _ = False
                ipv4only si =
                    si
                        { svcbInfoResolveInfos = filter (isIPv4 . rinfoIP) $ svcbInfoResolveInfos si
                        }
            let siss
                    | optDisableV6NS || disableV6 = map (map ipv4only) siss0
                    | otherwise = siss0
            case siss of
                [] -> do
                    putStrLn "No proper SVCB"
                    exitFailure
                sis : _ -> case sis of
                    [] -> do
                        putStrLn "No proper SVCB"
                        exitFailure
                    si : _ -> return $ toPipelineResolver $ modifyForDDR si

resolveDoX
    :: Options
    -> [ResolveInfo]
    -> IO [PipelineResolver]
resolveDoX opt ris = case makePersistentResolver $ optDoX opt of
    Nothing -> return [] -- udp
    Just persitResolver -> return (persitResolver <$> ris)

----------------------------------------------------------------

saveResumption :: FilePath -> MVar () -> TQueue (NameTag, String) -> NameTag -> ByteString -> IO ()
saveResumption file lock tq name bs = do
    case extractInfo of
        Nothing -> return ()
        Just info -> atomically $ writeTQueue tq (name, info)
    safeAppendFile file lock (C8.pack (fromNameTag name) <> " " <> BS16.encode bs <> "\n")
  where
    extractInfo
        | "QUIC" == nameTagProto name || "H3" == nameTagProto name =
            case deserialiseOrFail $ BL.fromStrict bs of
                Left _ -> Nothing
                Right (info :: QUIC.ResumptionInfo) ->
                    Just $ next (QUIC.isResumptionPossible info) (QUIC.is0RTTPossible info)
        | otherwise =
            case deserialiseOrFail $ BL.fromStrict bs of
                Left _ -> Nothing
                Right (_ :: TLS.SessionID, sd :: TLS.SessionData) ->
                    Just $ next True (TLS.is0RTTPossible sd)
    next res rtt0 = "Next(Resumption:" ++ ok res ++ ", 0-RTT:" ++ ok rtt0 ++ ")"
    ok True = "OK"
    ok False = "NG"

loadResumption :: FilePath -> IO [(NameTag, ByteString)]
loadResumption file = map toKV . C8.lines <$> C8.readFile file
  where
    toKV l = (toNameTag $ C8.unpack k, fromRight "" $ BS16.decode $ C8.drop 1 v)
      where
        (k, v) = BS.break (== 32) l

safeAppendFile :: FilePath -> MVar () -> ByteString -> IO ()
safeAppendFile file lock bs = withMVar lock $ \_ -> BS.appendFile file bs

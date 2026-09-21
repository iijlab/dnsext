{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Standing up a world of its own: clove as the primary for it,
--   bowline as the resolver for it, and a way to ask bowline questions.
--
--   What a scenario is made of lives in a directory of its own under
--   @test@ -- its zone files and the configurations of the two
--   programs, with @{{DIR}}@, @{{FILES}}@ and the ports left for this
--   to put in.  Nothing here talks to the network beyond the loopback,
--   and nothing it writes outlives the scenario.  The ports are asked
--   of the kernel rather than chosen, so that scenarios may run beside
--   each other and beside whatever else is on the machine.
module Harness (
    Scenario (..),
    withScenario,
    ask,
    Answer (..),
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, throwIO)
import Control.Monad (unless, void)
import Data.List (isPrefixOf, stripPrefix)
import System.Directory (
    createDirectoryIfMissing,
    getTemporaryDirectory,
    listDirectory,
    makeAbsolute,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Error (isDoesNotExistError)
import System.Process
import Text.Printf (printf)

import DNS.Do53.Client hiding (lookup)
import DNS.Do53.Internal
import DNS.Types hiding (Answer)

import Network.Socket

----------------------------------------------------------------

-- | A world which has been stood up: where its files are, and where its
--   resolver is listening.
data Scenario = Scenario
    { scenarioDir :: FilePath
    -- ^ Where this run keeps what it writes.  It holds the logs of both
    --   programs, which is where to look when a scenario does not come
    --   out as it should.
    , scenarioResolver :: PortNumber
    -- ^ The port bowline answers on.
    , scenarioPrimary :: PortNumber
    -- ^ The port clove answers on, for asking the primary directly.
    }

-- | What came back, reduced to what a scenario asks about.
data Answer = Answer
    { answerRcode :: RCODE
    , answerAuthentic :: Bool
    -- ^ The AD bit: whether the resolver is saying it validated this.
    , answerRRs :: [ResourceRecord]
    }
    deriving (Eq, Show)

----------------------------------------------------------------

-- | Standing the named scenario up for as long as the action runs.  The
--   name is the directory its files are in, under @test@.
withScenario :: String -> (Scenario -> IO a) -> IO a
withScenario name body = do
    [primaryPort, resolverPort, monitorPort] <- mapM (const freePort) [1 :: Int, 2, 3]
    -- Absolute: clove changes into its own directory once it has read
    -- its configuration, so a relative path in one is a path to
    -- somewhere else by the time it is used.
    files <- makeAbsolute $ "test" </> name
    -- The directory is named after a port nothing else has, so that two
    -- scenarios at once do not write over each other.
    withTempDir (show primaryPort) $ \dir -> do
        let fill =
                [ ("DIR", dir)
                , ("FILES", files)
                , ("PRIMARY_PORT", show primaryPort)
                , ("RESOLVER_PORT", show resolverPort)
                , ("MONITOR_PORT", show monitorPort)
                ]
        createDirectoryIfMissing True (dir </> "clove")
        configure fill (files </> "clove.conf") (dir </> "clove.conf")
        configure fill (files </> "bowline.conf") (dir </> "bowline.conf")
        -- clove first: bowline is given a trust anchor made from the key
        -- clove generates, so the key has to exist before bowline starts.
        withDaemon dir "clove" ["--insecure", dir </> "clove.conf"] $ do
            waitFor "clove" $ answered primaryPort "example." SOA
            writeFile (dir </> "anchor.zone") =<< trustAnchor (dir </> "clove" </> "example")
            withDaemon dir "bowline" [dir </> "bowline.conf"] $ do
                waitFor "bowline" $ answered resolverPort "www.example." A
                body
                    Scenario
                        { scenarioDir = dir
                        , scenarioResolver = resolverPort
                        , scenarioPrimary = primaryPort
                        }

-- | A configuration file of the scenario, with this run's directory and
--   ports put in where it leaves room for them.
configure :: [(String, String)] -> FilePath -> FilePath -> IO ()
configure fill src dst = writeFile dst . substitute fill =<< readFile src

substitute :: [(String, String)] -> String -> String
substitute fill = go
  where
    go [] = []
    go s@(c : cs) = case [(k, v) | (k, v) <- fill, Just _ <- [stripPrefix (brace k) s]] of
        (k, v) : _ -> v ++ go (drop (length (brace k)) s)
        [] -> c : go cs
    brace k = "{{" ++ k ++ "}}"

-- | The DS of the zone's key signing key, as a trust anchor for bowline
--   to start from.  clove writes everything a DS is made of into the key
--   file, so it is read from there rather than worked out again.
trustAnchor :: FilePath -> IO String
trustAnchor zoneDir = do
    names <- listDirectory zoneDir
    case [n | n <- names, ".ksk" `isSuffix` n] of
        [] -> fail $ "no key signing key in " ++ zoneDir
        ksk : _ -> do
            fields <- keyFields <$> readFile (zoneDir </> ksk)
            let field k = maybe (fail $ "no " ++ k ++ " in " ++ ksk) pure $ lookup k fields
            tag <- field "KeyTag"
            alg <- field "Algorithm"
            dig <- field "DigestAlgo"
            hash <- field "Digest"
            pure $ printf "example.\t3600\tIN\tDS\t%s %s %s %s\n" tag alg dig hash
  where
    isSuffix s x = reverse s `isPrefixOf` reverse x
    -- "KeyTag:     6109" and "Algorithm:  15 # ED25519": the name up to
    -- the colon, and the first word after it.
    keyFields = concatMap oneField . lines
    oneField l = case break (== ':') l of
        (k, ':' : rest) -> [(k, w) | w : _ <- [words rest]]
        _ -> []

----------------------------------------------------------------

-- | Asking bowline, with DO set so that it says whether it validated.
ask :: Scenario -> Domain -> TYPE -> IO Answer
ask sc dom typ = do
    er <- udpResolver (resolveInfo $ scenarioResolver sc) (Question dom typ IN) ctl
    case er of
        Left e -> throwIO e
        Right Reply{..} ->
            pure
                Answer
                    { answerRcode = rcode replyDNSMessage
                    , answerAuthentic = authenData $ flags replyDNSMessage
                    , answerRRs = answer replyDNSMessage
                    }
  where
    ctl = rdFlag FlagSet <> doFlag FlagSet <> cdFlag FlagClear

resolveInfo :: PortNumber -> ResolveInfo
resolveInfo port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoUDPRetry = 1
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 3000000}
        }

-- | Whether a server on this port has an answer for this question yet.
answered :: PortNumber -> Domain -> TYPE -> IO Bool
answered port dom typ = do
    er <- udpResolver (resolveInfo port) (Question dom typ IN) (rdFlag FlagSet)
    pure $ case er of
        Right Reply{..} -> rcode replyDNSMessage == NoErr
        Left _ -> False

----------------------------------------------------------------

-- | A daemon for as long as the action runs.  Killed on the way out
--   whether the action finished or threw, and what it says goes to a
--   file in the scenario's directory rather than into the test's
--   output, where it is there to be read when a scenario does not come
--   out as it should.
withDaemon :: FilePath -> String -> [String] -> IO a -> IO a
withDaemon dir prog args body = withFile (dir </> prog ++ ".log") WriteMode $ \h ->
    bracket (start h) stop (const body)
  where
    start h =
        (\(_, _, _, ph) -> ph)
            <$> createProcess (proc prog args){std_out = UseHandle h, std_err = UseHandle h}
    stop ph = terminateProcess ph >> void (waitForProcess ph)

-- | Waiting for a daemon to be able to answer.  Each of them reads its
--   configuration, binds its sockets and loads or signs a zone before it
--   can, and how long that takes is the machine's business.
waitFor :: String -> IO Bool -> IO ()
waitFor what ready = go (300 :: Int)
  where
    go 0 = fail $ what ++ " did not start answering"
    go n = do
        ok <- ready
        unless ok $ threadDelay 100000 >> go (n - 1)

----------------------------------------------------------------

-- | A port nothing is listening on.  Asked of the kernel and given
--   back, which leaves the moment between here and the daemon binding
--   it; there is no way to hand a bound socket to another program.
freePort :: IO PortNumber
freePort = bracket open close socketPort
  where
    open = do
        s <- socket AF_INET Stream defaultProtocol
        setSocketOption s ReuseAddr 1
        bind s $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        pure s

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name = bracket make remove
  where
    make = do
        tmp <- getTemporaryDirectory
        let dir = tmp </> "dnsext-scenario-" ++ name
        removeIfThere dir
        createDirectoryIfMissing True dir
        pure dir
    remove = removeIfThere
    removeIfThere d =
        removeDirectoryRecursive d `catch` \e ->
            unless (isDoesNotExistError e) $ throwIO e

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Standing up a world of its own: clove as the primaries for it,
--   bowline as the resolver for it, and a way to ask bowline questions.
--
--   The world has a root of its own.  One clove serves that root and
--   another serves the zones below it, and bowline is told by
--   @auth-port@ which port to ask authoritative servers on, so that a
--   scenario's delegations are walked down rather than stepped over and
--   nothing needs a privileged port.  The two primaries share the port
--   and are told apart by address, which is what one loopback interface
--   has room for: a parent and a child, which is what a scenario about
--   a delegation is made of.
--
--   What a scenario is made of lives in a directory of its own under
--   @test@ -- its zone files and the configurations of the three
--   programs, with @{{DIR}}@, the addresses and the ports left for this
--   to put in.  All of it is copied into a directory of this run's own
--   before anything starts, so that what a run was given can be read
--   afterwards beside what it wrote.  Nothing here talks to the network
--   beyond the loopback, and nothing it writes outlives the scenario.
--   The ports are asked of the kernel rather than chosen, so that
--   scenarios may run beside each other and beside whatever else is on
--   the machine.
module Harness (
    Scenario (..),
    withScenario,
    ask,
    askChecking,
    Answer (..),
    Server (..),
    asked,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, throwIO)
import Control.Monad (unless, void, when)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
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
import DNS.SEC (addResourceDataForDNSSEC)
import DNS.Types hiding (Answer)

import Network.Socket

----------------------------------------------------------------

-- | A world which has been stood up: where its files are, and where its
--   resolver is listening.
data Scenario = Scenario
    { scenarioDir :: FilePath
    -- ^ Where this run keeps what it writes.  It holds the
    --   configurations as they were filled in and the logs of all three
    --   programs, which is where to look when a scenario does not come
    --   out as it should.
    , scenarioResolver :: PortNumber
    -- ^ The port bowline answers on.
    , scenarioAuth :: PortNumber
    -- ^ The port both primaries answer on, for asking one directly.
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

-- | Where the root of the scenario's world is.  Every machine has this
--   one.
rootAddr :: String
rootAddr = "127.0.0.1"

-- | A second address on the loopback, for the servers below the root.
--   Linux has the whole of 127\/8 and so needs nothing of IPv6, which
--   is worth avoiding where a builder may have none; elsewhere -- macOS
--   among them -- 127.0.0.1 is the only IPv4 address there is, and the
--   second address has to be @::1@.  Which of the two it is, is asked
--   of the kernel rather than of the operating system's name.
authAddr :: IO String
authAddr = do
    second <- bindable AF_INET "127.0.0.2"
    pure $ if second then "127.0.0.2" else "::1"

-- | Whether this machine will let a socket have this address.
bindable :: Family -> String -> IO Bool
bindable family addr = do
    ais <- getAddrInfo (Just hints) (Just addr) (Just "0")
    case ais of
        [] -> pure False
        ai : _ ->
            bracket (openSocket ai) close (\s -> bind s (addrAddress ai) >> pure True)
                `catch` \e -> const (pure False) (e :: IOError)
  where
    hints = defaultHints{addrFamily = family, addrSocketType = Datagram, addrFlags = [AI_NUMERICHOST]}

-- | How a zone's name is written where an address is expected of it:
--   a scenario's zone file says @ns.example. IN {{AUTH_ADDR_RR}}@ and
--   gets an A or a AAAA according to which address this run is using.
addrRR :: String -> String
addrRR addr
    | ':' `elem` addr = "AAAA\t" ++ addr
    | otherwise = "A\t" ++ addr

----------------------------------------------------------------

-- | Standing the named scenario up for as long as the action runs.  The
--   name is the directory its files are in, under @test@.
withScenario :: String -> (Scenario -> IO a) -> IO a
withScenario name body = do
    -- The types DNSSEC adds are not in the dictionaries until somebody
    -- puts them there, and until they are, an RRSIG in an answer is an
    -- unknown type with a string of hex in it and DNSKEY is a word
    -- nothing can read.  Every scenario here has signed zones in it.
    -- Registering a type twice is writing the same entry twice.
    runInitIO addResourceDataForDNSSEC
    [authPort, resolverPort, monitorPort] <- mapM (const freePort) [1 :: Int, 2, 3]
    auth <- authAddr
    files <- makeAbsolute $ "test" </> name
    -- The directory is named after a port nothing else has, so that two
    -- scenarios at once do not write over each other.
    withTempDir (show authPort) $ \dir -> do
        let fill =
                [ ("DIR", dir)
                , ("ROOT_ADDR", rootAddr)
                , ("AUTH_ADDR", auth)
                , ("AUTH_ADDR_RR", addrRR auth)
                , ("AUTH_PORT", show authPort)
                , ("RESOLVER_PORT", show resolverPort)
                , ("MONITOR_PORT", show monitorPort)
                ]
        mapM_ (createDirectoryIfMissing True . (dir </>)) ["clove", "root"]
        copyIn fill files dir
        writeFile (dir </> "root.hints") rootHints
        -- A scenario which needs clove to serve what clove would
        -- otherwise refuse says so in a file of its own, so that
        -- everything about a scenario is in the scenario's directory.
        cloveArgs <- readArgs $ dir </> "clove.args"
        rootArgs <- readArgs $ dir </> "root.args"
        -- The zones below the root first: the root vouches for them
        -- with a DS, and a DS is made of a key which does not exist
        -- until the zone it belongs to has been served once.
        withDaemon dir "clove" "clove" (cloveArgs ++ [dir </> "clove.conf"]) $ do
            waitFor "clove" $ answered auth authPort "example." SOA
            fillDS (dir </> "clove") (dir </> "root.zone")
            withDaemon dir "root" "clove" (rootArgs ++ [dir </> "root.conf"]) $ do
                waitFor "root" $ answered rootAddr authPort "." SOA
                writeFile (dir </> "anchor.zone") =<< trustAnchor (dir </> "root" </> "root.")
                withDaemon dir "bowline" "bowline" [dir </> "bowline.conf"] $ do
                    -- The root, and nothing of the scenario's own: what
                    -- is being waited for is bowline listening and
                    -- reaching its hint, and a scenario about a zone
                    -- which does not resolve should fail as a test
                    -- rather than as a world which never came up.
                    waitFor "bowline" $ answered rootAddr resolverPort "." SOA
                    body
                        Scenario
                            { scenarioDir = dir
                            , scenarioResolver = resolverPort
                            , scenarioAuth = authPort
                            }

-- | The scenario's own files, with this run's directory, addresses and
--   ports put in where they leave room for them.
copyIn :: [(String, String)] -> FilePath -> FilePath -> IO ()
copyIn fill from to = mapM_ one =<< listDirectory from
  where
    one n = do
        isFile <- doesFileExist (from </> n)
        when isFile $ writeFile (to </> n) . substitute fill =<< readFile (from </> n)

-- | The arguments a scenario wants a primary started with, where it
--   wants any.
readArgs :: FilePath -> IO [String]
readArgs path = (concatMap words . lines <$> readFile path) `catch` missing
  where
    missing e
        | isDoesNotExistError e = pure []
        | otherwise = throwIO e

substitute :: [(String, String)] -> String -> String
substitute fill = go
  where
    go [] = []
    go s@(c : cs) = case [(k, v) | (k, v) <- fill, Just _ <- [stripPrefix (brace k) s]] of
        (k, v) : _ -> v ++ go (drop (length (brace k)) s)
        [] -> c : go cs
    brace k = "{{" ++ k ++ "}}"

----------------------------------------------------------------

-- | Where bowline starts from.  It is replaced by what the root zone
--   actually says as soon as bowline asks it, so all this has to do is
--   name a server and say where it is.
rootHints :: String
rootHints =
    unlines
        [ ".\t3600\tIN\tNS\tns.root."
        , "ns.root.\t3600\tIN\tA\t" ++ rootAddr
        ]

-- | Putting into the root zone the DS of each zone the primary below it
--   signs.  A scenario writes @example. IN DS {{DS example}}@, naming
--   the zone as its primary's configuration does; a delegation which is
--   meant to be insecure simply says nothing.
fillDS :: FilePath -> FilePath -> IO ()
fillDS cloveDir rootZone = do
    -- Read all of it before writing any of it: this is the one file
    -- which is its own source.
    before <- readFile rootZone
    length before `seq` pure ()
    fill <- mapM entry $ dsWanted before
    writeFile rootZone (substitute fill before)
  where
    -- The primary answers as soon as its first zone is ready, and a
    -- scenario may have given it several, so the key of the zone being
    -- asked about is not certain to exist yet.  Only the zones the root
    -- zone actually asks for a DS of are waited for: one which is meant
    -- to be an insecure delegation says nothing here and is never
    -- signed, so waiting for its key would be waiting for ever.
    entry z = do
        rd <- waitForValue ("the key of " ++ z) $ dsRdata (cloveDir </> z)
        pure ("DS " ++ z, rd)

-- | The zones a root zone asks for a DS of, in the order it asks.
dsWanted :: String -> [String]
dsWanted = go
  where
    go [] = []
    go s@(_ : cs) = case stripPrefix "{{DS " s of
        Just rest | (z, '}' : '}' : more) <- break (== '}') rest -> z : go more
        _ -> go cs

-- | The DS of the root of the scenario's world, as the trust anchor
--   bowline starts from.
trustAnchor :: FilePath -> IO String
trustAnchor zoneDir =
    maybe (fail $ "no key signing key in " ++ zoneDir) (pure . printf ".\t3600\tIN\tDS\t%s\n")
        =<< dsRdata zoneDir

-- | What a DS of this zone is made of, or nothing where the zone has
--   not been signed -- which is either because it never will be or
--   because it has not been yet.  clove writes everything a DS needs
--   into the key file, so it is read from there rather than worked out
--   again.
dsRdata :: FilePath -> IO (Maybe String)
dsRdata zoneDir = do
    names <-
        listDirectory zoneDir `catch` \e ->
            if isDoesNotExistError e then pure [] else throwIO e
    case [n | n <- names, ".ksk" `isSuffix` n] of
        [] -> pure Nothing
        ksk : _ -> do
            fields <- keyFields <$> readFile (zoneDir </> ksk)
            let field k = maybe (fail $ "no " ++ k ++ " in " ++ ksk) pure $ lookup k fields
            Just . unwords <$> mapM field ["KeyTag", "Algorithm", "DigestAlgo", "Digest"]
  where
    isSuffix s x = reverse s `isPrefixOf` reverse x
    -- "KeyTag:     6109" and "Algorithm:  15 # ED25519": the name up to
    -- the colon, and the first word after it.
    keyFields = concatMap oneField . lines
    oneField l = case break (== ':') l of
        (k, ':' : rest) -> [(k, w) | w : _ <- [words rest]]
        _ -> []

----------------------------------------------------------------

-- | One of the two primaries of the scenario's world.
data Server
    = TheRoot
    | ThePrimary
    deriving (Eq, Show)

-- | What a server was asked, in the order it was asked it.
--
--   This is the one thing about a scenario which cannot be seen in the
--   answer: whether bowline told a server more than it needed to know,
--   how many times it went back, which of them it went to.  clove
--   writes a line for every query it answers, so the questions are read
--   out of its log -- which means a scenario wanting them has to set
--   @log-level: DEBUG@ for the server in question, the default here
--   being quieter than that.
asked :: Scenario -> Server -> IO [(Domain, TYPE)]
asked sc server = do
    -- A server logging at the default level writes no queries at all,
    -- and an empty list of them is what every assertion about what was
    -- asked would then quietly agree with.  Say so instead.
    conf <- readFile (scenarioDir sc </> confOf server)
    unless ("log-level: DEBUG" `isInfixOf` conf) $
        fail $
            confOf server ++ ": asked needs log-level: DEBUG to have anything to read"
    mapMaybe oneQuestion . lines <$> readFile (scenarioDir sc </> logOf server)
  where
    logOf TheRoot = "root.log"
    logOf ThePrimary = "clove.log"
    confOf TheRoot = "root.conf"
    confOf ThePrimary = "clove.conf"

-- | clove writes @\"a.b.example.\" A from 127.0.0.1\/UDP@ for each
--   query it answers.  Whatever else is in the log is not one.
oneQuestion :: String -> Maybe (Domain, TYPE)
oneQuestion ('"' : rest)
    | (name, '"' : more) <- break (== '"') rest
    , typ : "from" : _ <- words more =
        Just (fromRepresentation name, read typ)
oneQuestion _ = Nothing

----------------------------------------------------------------

-- | Asking bowline, with DO set so that it says whether it validated,
--   and with CD clear so that it is doing the validating.
ask :: Scenario -> Domain -> TYPE -> IO Answer
ask = askWith (cdFlag FlagClear)

-- | The same question with CD set: RFC 4035 Sec 3.2.2 has the resolver
--   hand over what it has without validating it, for a client which
--   would rather check for itself.
askChecking :: Scenario -> Domain -> TYPE -> IO Answer
askChecking = askWith (cdFlag FlagSet)

askWith :: QueryControls -> Scenario -> Domain -> TYPE -> IO Answer
askWith cd sc dom typ = do
    -- Over UDP and then over TCP where the answer did not fit, which is
    -- what a client does and what a scenario wants: with DO set an
    -- answer of any size stops fitting quickly, and a truncated one
    -- looks like a missing record rather than like a truncated one.
    er <- udpTcpResolver (resolveInfo rootAddr $ scenarioResolver sc) (Question dom typ IN) ctl
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
    ctl = rdFlag FlagSet <> doFlag FlagSet <> cd

resolveInfo :: String -> PortNumber -> ResolveInfo
resolveInfo addr port =
    defaultResolveInfo
        { rinfoIP = fromString addr
        , rinfoPort = port
        , rinfoUDPRetry = 1
        , -- A chain of CNAMEs with a signature on each of them is past
          -- the 2048 a resolver is given by default.
          rinfoVCLimit = 8 * 1024
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 3000000}
        }

-- | Whether a server at this address has an answer for this question
--   yet.
answered :: String -> PortNumber -> Domain -> TYPE -> IO Bool
answered addr port dom typ = do
    er <- udpResolver (resolveInfo addr port) (Question dom typ IN) (rdFlag FlagSet)
    pure $ case er of
        Right Reply{..} -> rcode replyDNSMessage == NoErr
        Left _ -> False

----------------------------------------------------------------

-- | A daemon for as long as the action runs.  Killed on the way out
--   whether the action finished or threw, and what it says goes to a
--   file in the scenario's directory rather than into the test's
--   output, where it is there to be read when a scenario does not come
--   out as it should.  The two primaries are the same program, so the
--   log is named after the part being played rather than after it.
withDaemon :: FilePath -> String -> String -> [String] -> IO a -> IO a
withDaemon dir part prog args body = withFile (dir </> part ++ ".log") WriteMode $ \h ->
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
waitFor what ready = void $ waitForValue what $ boolToMaybe <$> ready
  where
    boolToMaybe ok = if ok then Just () else Nothing

-- | The same wait, for something with a value: whatever is being waited
--   for is asked for until it is there.
waitForValue :: String -> IO (Maybe a) -> IO a
waitForValue what get = go (300 :: Int)
  where
    go 0 = fail $ what ++ ": waited for it and it never came"
    go n =
        get >>= \mx -> case mx of
            Just x -> pure x
            Nothing -> threadDelay 100000 >> go (n - 1)

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

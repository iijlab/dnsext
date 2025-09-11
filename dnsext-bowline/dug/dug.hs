{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically, newTQueueIO, tryReadTQueue)
import Control.Monad (unless, when)
import DNS.Do53.Client (
    FlagOp (..),
    QueryControls,
    adFlag,
    cdFlag,
    doFlag,
    rdFlag,
 )
import qualified DNS.Do53.Client as Do53
import DNS.Do53.Internal (fromNameTag)
import DNS.DoX.Client
import DNS.SEC (addResourceDataForDNSSEC)
import DNS.SVCB (ALPN, addResourceDataForSVCB)
import DNS.Types (
    CLASS (..),
    DNSMessage,
    Domain,
    Question (..),
    TYPE (..),
    allTYPEs,
    fromRepresentation,
    runInitIO,
 )
import qualified DNS.Types as DNS
import Data.Bits
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import Data.Char (toLower)
import Data.Either (rights)
import Data.Function (on)
import Data.Functor
import Data.IP (IP (..), fromIPv4, fromIPv6b)
import Data.List (groupBy, intercalate, isPrefixOf, nub, partition, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.UnixTime as T
import Network.Socket (HostName, PortNumber)
import System.Console.ANSI.Types
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified DNS.Log as Log

import Iterative (getRootV6, iterativeQuery)
import JSON (showJSON)
import Output (OutputFlag (..), pprResult)
import Recursive (recursiveQuery)
import SocketUtil (checkDisableV6)
import Types

version :: String
version = "20250812"

----------------------------------------------------------------

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['h']
        ["help"]
        (NoArg (\opts -> opts{optHelp = True}))
        "print help"
    , Option
        ['i']
        ["iterative"]
        (NoArg (\opts -> opts{optIterative = True}))
        "resolve iteratively"
    , Option
        ['4']
        ["ipv4"]
        (NoArg (\opts -> opts{optDisableV6NS = True}))
        "disable IPv6 NS"
    , Option
        ['p']
        ["port"]
        (ReqArg (\port opts -> opts{optPort = Just port}) "<port>")
        "specify port number"
    , Option
        ['d']
        ["dox"]
        ( ReqArg
            (\dox opts -> opts{optDoX = convDoX dox})
            "<proto>"
        )
        "enable DoX"
    , Option
        ['f']
        ["format"]
        ( ReqArg
            (\fmt opts -> opts{optFormat = convOutputFlag fmt})
            "<format>"
        )
        "set the output format"
    , Option
        ['v']
        ["verbose"]
        (NoArg (\opts -> opts{optVerboseLevel = succ $ optVerboseLevel opts}))
        "cumulatively increase the verbosity"
    , Option
        ['R']
        ["resumption-file"]
        ( ReqArg
            (\file opts -> opts{optResumptionFile = Just file})
            "<file>"
        )
        "specify a file to save resumption information"
    , Option
        ['Z']
        ["0rtt"]
        (NoArg (\opts -> opts{opt0RTT = True}))
        "use 0-RTT (aka early data)"
    , Option
        ['l']
        ["keylog-file"]
        ( ReqArg
            (\file opts -> opts{optKeyLogFile = Just file})
            "<file>"
        )
        "specify a file to save TLS main secret keys"
    , Option
        ['e']
        ["validate"]
        (NoArg (\o -> o{optValidate = True}))
        "validate server's certificate"
    ]

----------------------------------------------------------------

main :: IO ()
main = do
    runInitIO $ do
        {- Override the parser behavior to accept the extended TYPE.
           Therefore, this action is required prior to reading the TYPE. -}
        addResourceDataForDNSSEC
        addResourceDataForSVCB
    (deprecated, compatH, args0) <- getArgs <&> handleDeprecatedVerbose
    (args, opts1@Options{..}) <- getArgsOpts args0 <&> fmap compatH
    when optHelp $ do
        msg <- help
        putStr $ usageInfo msg options
        putStr "\n"
        putStrLn "  <proto>     = auto | tcp | dot | doq | h2 | h2c | h3"
        putStrLn "  <format>    = multi | json"
        putStrLn "  <verbosity> = 0 | 1 | 2 | 3"
        exitSuccess
    ------------------------
    deprecated
    (at, port, qs, runLogger, putLnSTM, putLinesSTM, killLogger) <- cookOpts args opts1
    when (null qs) $ do
        putStrLn "Usage: dug [options] [@server]* [name [query-type] [query-control]*]+"
        exitFailure
    let putLn = atomically . putLnSTM
        putLines a b c = atomically $ putLinesSTM a b c
    void $ forkIO runLogger
    t0 <- T.getUnixTime
    tq <- newTQueueIO
    ------------------------
    if optIterative
        then do
            target <- checkIterative at qs
            opts <- checkFallbackV4 opts1 =<< getRootV6
            iterativeQuery putLn putLines target opts
        else do
            let mserver = map (drop 1) at
            ips <- resolveServers opts1 mserver
            opts <- checkFallbackV4 opts1 [(ip, 53) | (ip, _) <- ips]
            recursiveQuery ips port putLnSTM putLinesSTM qs opts tq
    ------------------------
    when (optFormat `notElem` [Short, JSONstyle]) $ putTime t0 putLines
    killLogger
    sentinel tq
    deprecated
    exitSuccess
  where
    sentinel tq = do
        xs <- readQ
        let summary = map Prelude.unzip $ groupBy ((==) `on` fst) $ nub $ sort xs
        mapM_ printIt summary
      where
        printIt (tag : _, ds) = putStrLn $ fromNameTag tag ++ ": " ++ intercalate ", " (reverse ds)
        printIt (_, _) = error "printIt"
        readQ = do
            mx <- atomically $ tryReadTQueue tq
            case mx of
                Nothing -> return []
                Just x -> do
                    xs <- readQ
                    return (x : xs)

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
checkFallbackV4 :: Options -> [(IP, PortNumber)] -> IO Options
checkFallbackV4 opt addrs
    | optDisableV6NS opt =  pure opt
    | otherwise          =  checkDisableV6 [a | (a, _) <- addrs] <&> \d -> opt{optDisableV6NS = d}
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
resolveServers :: Options -> [HostName] -> IO [(IP, Maybe HostName)]
resolveServers Options{..} hs = concat <$> mapM toNumeric hs
  where
    toNumeric sname
        | Just ip <- readMaybe sname  = pure [(IPv4 ip, Nothing)]
        | Just ip <- readMaybe sname  = when optDisableV6NS (fail $ "IPv6 host address with '-4': " ++ sname) $> [(IPv6 ip, Nothing)]
    toNumeric sname = Do53.withLookupConf Do53.defaultLookupConf $ \env -> do
        let dom = fromRepresentation sname
        eA <- fmap (fmap (IPv4 . DNS.a_ipv4)) <$> Do53.lookupA env dom
        eAAAA <- sequence [ fmap (fmap (IPv6 . DNS.aaaa_ipv6)) <$> Do53.lookupAAAA env dom | not optDisableV6NS ]
        let as = concat $ rights $ eA : eAAAA
        when (null as) (fail $ show eA) $> map (, Just sname) as
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

cookOpts
    :: [String]
    -> Options
    -> IO
        ( [String]
        , PortNumber
        , [(Question, QueryControls)]
        , IO ()
        , DNSMessage -> STM ()
        , Log.PutLines STM
        , IO ()
        )
cookOpts args opt@Options{..} = do
    let (at, dtq) = partition ("@" `isPrefixOf`) args
    qs <- getQueries dtq
    port <- getPort optPort optDoX
    (runLogger, putLines, _, killLogger) <- Log.new Log.Stdout (logLevel opt)
    let putLn = mkPutline optFormat putLines
    return (at, port, qs, runLogger, putLn, putLines, killLogger)

----------------------------------------------------------------

checkIterative
    :: [String]
    -> [(Question, QueryControls)]
    -> IO (Question, QueryControls)
checkIterative at qs = do
    unless (null at) $ do
        putStrLn "@ cannot used with '-i'"
        exitFailure
    case qs of
        [] -> do
            putStrLn "domain must be specified"
            exitFailure
        [q] -> return q
        _ -> do
            putStrLn "multiple domains must not be specified"
            exitFailure

----------------------------------------------------------------

putTime
    :: T.UnixTime
    -> (Log.Level -> Maybe Color -> [String] -> IO ())
    -> IO ()
putTime t0 putLines = do
    t1 <- T.getUnixTime
    let T.UnixDiffTime s u = t1 `T.diffUnixTime` t0
    let sec = if s /= 0 then show s ++ "sec " else ""
        tm =
            ";; "
                ++ sec
                ++ show (u `div` 1000)
                ++ "usec"
    putLines Log.WARN (Just Green) [tm]

----------------------------------------------------------------

mkPutline
    :: OutputFlag
    -> (Log.Level -> Maybe Color -> [String] -> STM ())
    -> DNSMessage
    -> STM ()
mkPutline format putLinesSTM msg = putLinesSTM Log.WARN Nothing [res msg]
  where
    res = case format of
        JSONstyle -> showJSON
        Singleline -> pprResult []
        Short -> pprResult [Short]
        Multiline -> pprResult [Multiline]

----------------------------------------------------------------

getArgsOpts :: [String] -> IO ([String], Options)
getArgsOpts args = case getOpt Permute options args of
    (o, n, []) -> return (n, foldl (flip id) defaultOptions o)
    (_, _, errs) -> do
        mapM_ putStr errs
        exitFailure

----------------------------------------------------------------

getQueries :: [String] -> IO [(Question, QueryControls)]
getQueries xs0 = loop xs0 id
  where
    loop [] build = return $ build []
    loop xs build = do
        (q, ys) <- getQuery xs
        loop ys (build . (q :))

-- Question d t IN
getQuery :: [String] -> IO ((Question, QueryControls), [String])
getQuery [] = do
    putStrLn "never reached"
    exitFailure
getQuery xs = do
    (queryName, rev, xs1) <- getQueryName xs
    (queryType', xs2) <- getQueryType xs1
    let queryType
            | rev = PTR
            | otherwise = queryType'
    (queryControls, ys) <- getQueryControls xs2
    return ((Question queryName queryType IN, queryControls), ys)

getQueryName :: [String] -> IO (Domain, Bool, [String])
getQueryName [] = do
    putStrLn "Query name must be specified"
    exitFailure
getQueryName (queryName : ys)
    | Just (IPv4 ip4) <- readMaybe queryName = do
        let name = intercalate "." (map show $ reverse $ fromIPv4 ip4) ++ ".in-addr.arpa"
        return (fromRepresentation name, True, ys)
    | Just (IPv6 ip6) <- readMaybe queryName = do
        print $ fromIPv6b ip6
        let name = intercalate "." (reverse [printf "%x" h | x <- fromIPv6b ip6, h <- [x `unsafeShiftR` 4, x .&. 0xf]]) ++ ".ip6.arpa"
        return (fromRepresentation name, True, ys)
    | '.' `elem` queryName = return (fromRepresentation queryName, False, ys)
    | otherwise = do
        putStrLn $ show queryName ++ " does not contain '.'"
        exitFailure

getQueryType :: [String] -> IO (TYPE, [String])
getQueryType [] = return (A, [])
getQueryType (queryType : ys)
    | '.' `elem` queryType = return (A, queryType : ys)
    | "+" `isPrefixOf` queryType = return (A, queryType : ys)
    | Just qt <- readMaybe queryType = return (qt, ys)
    | otherwise = do
        putStrLn $ "Type " ++ queryType ++ " is not supported"
        exitFailure

getQueryControls :: [String] -> IO (QueryControls, [String])
getQueryControls [] = return (mempty, [])
getQueryControls xs = do
    let (queryControls', ys) = span ("+" `isPrefixOf`) xs
    queryControls <- mconcat <$> mapM toFlag queryControls'
    return (queryControls, ys)

----------------------------------------------------------------

getPort :: Maybe String -> ALPN -> IO PortNumber
getPort Nothing optDoX = return $ doxPort optDoX
getPort (Just x) _ = case readMaybe x of
    Just p -> return p
    Nothing -> do
        putStrLn $ "Port " ++ x ++ " is illegal"
        exitFailure

{- FOURMOLU_DISABLE -}
convDoX :: String -> ShortByteString
convDoX dox = case dox' of
  "http2" -> "h2"
  "http3" -> "h3"
  "quic"  -> "doq"
  "tls"   -> "dot"
  x       -> Short.toShort x
  where
    dox' = C8.pack dox

convOutputFlag :: String -> OutputFlag
convOutputFlag "json"  = JSONstyle
convOutputFlag "multi" = Multiline
convOutputFlag "short" = Short
convOutputFlag _       = Singleline

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
handleDeprecatedVerbose :: [String] -> (IO (), Options -> Options, [String])
handleDeprecatedVerbose args0 = case reverse cs of
    []    -> (pure (),      id, args0)
    ca:_  -> (banner , handler, args1)
      where
        (n, handler) = fromMaybe (-1 {- never reach-}, id) $ lk ca
        banner = deprecatedVerboseBanner n ca
  where
    lk = (`lookup` deprecatedVerboseTable)
    (cs, args1) = partition (isJust . lk) args0
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
deprecatedVerboseBanner :: Int -> String -> IO ()
deprecatedVerboseBanner n ca =
    putStr $ unlines $ ["", border] ++ ftexts ++ [border, ""]
  where
    texts =
        [ ""
        , "WARNING: DEPRECATED-STYLE switch '" ++ ca ++"' WILL BE REMOVED in a future release!!" ]
        ++
        [ "         use '" ++ "-" ++ replicate n 'v' ++ "' instead of this!" | n > 0 ]
        ++
        [ "" ]
    wtexts = [(length t, t) | t <- texts]
    twidth = maximum [w | (w, _) <- wtexts]
    bg = "**  "
    ed = "  **"
    ftexts = [ bg ++ t ++ replicate (twidth - w) ' '  ++ ed | (w, t) <- wtexts]
    border = replicate (length bg + twidth + length ed) '*'
{- FOURMOLU_ENABLE -}

deprecatedVerboseTable :: [(String, (Int, Options -> Options))]
deprecatedVerboseTable =
    [ ("-v" ++ n, (nn, \opts -> opts{optVerboseLevel = nn}))
    | nn <- [0 .. 3]
    , let n = show nn
    ]

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
toFlag :: String -> IO QueryControls
toFlag "+rec"       = return $ rdFlag FlagSet
toFlag "+recurse"   = return $ rdFlag FlagSet
toFlag "+norec"     = return $ rdFlag FlagClear
toFlag "+norecurse" = return $ rdFlag FlagClear
toFlag "+dnssec"    = return $ doFlag FlagSet
toFlag "+nodnssec"  = return $ doFlag FlagClear
toFlag "+rdflag"    = return $ rdFlag FlagSet
toFlag "+nordflag"  = return $ rdFlag FlagClear
toFlag "+doflag"    = return $ doFlag FlagSet
toFlag "+nodoflag"  = return $ doFlag FlagClear
toFlag "+cdflag"    = return $ cdFlag FlagSet
toFlag "+nocdflag"  = return $ cdFlag FlagClear
toFlag "+adflag"    = return $ adFlag FlagSet
toFlag "+noadflag"  = return $ adFlag FlagClear
toFlag x            = do
    putStrLn $ "Unrecognized query control " ++ x
    exitFailure
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

help :: IO String
help = do
    types <- intercalate " | " . map (map toLower) <$> allTYPEs
    return $
        intercalate
            "\n"
            [ "Version: " ++ version
            , "Usage: dug [options] [@server]* [name [query-type] [query-control]*]+"
            , ""
            , "query-type: " ++ types
            , ""
            , "query-control:"
            , "  +[no]rdflag: [un]set RD (Recursion Desired) bit, +[no]rec[curse]"
            , "  +[no]doflag: [un]set DO (DNSSEC OK) bit, +[no]dnssec"
            , "  +[no]cdflag: [un]set CD (Checking Disabled) bit"
            , "  +[no]adflag: [un]set AD (Authentic Data) bit"
            , ""
            , "options:"
            ]

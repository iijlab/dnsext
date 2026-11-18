{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebAPI (
    bindAPIs,
    run,
) where

import qualified Control.Exception as E
import Control.Monad
import Data.ByteString ()
import Data.Functor
import Data.String
import qualified Network.HTTP.Types as HTTP
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)

import DNS.Iterative.Server (withLocationIOE)

import Config
import SocketUtil (ainfosSkipError)
import Types

doStats :: Control -> IO Response
doStats Control{..} = responseBuilder HTTP.ok200 [] <$> getStats

doWStats :: Control -> IO Response
doWStats Control{..} = responseBuilder HTTP.ok200 [] <$> getWStats

{- FOURMOLU_DISABLE -}
doHelp :: IO Response
doHelp = return $ responseBuilder HTTP.ok200 [] txt
  where
    txt = fromString $ unlines $ "WebAPI help:" : "" : map (uncurry hline) helps
    helps =
        [ ("/metrics"     , "returns metrics info")
        , ("/wstats"      , "returns worker thread info")
        , ("/reopen-log"  , "reopen logfile when file logging")
        , ("/reload"      , "reload bowline without keeping cache")
        , ("/keep-cache"  , "reload bowline with keeping cache")
        , ("/quit"        , "quit bowline")
        , ("/help"        , "show this help texts")
        ]
    hline name note = name ++ replicate (width - length name) ' ' ++ note
    width = maximum (0 : map (length . fst) helps) + margin
    margin = 3
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
app :: Control -> Application
app mng req sendResp = getResp >>= sendResp
  where
    getResp
        | requestMethod req == HTTP.methodGet = case rawPathInfo req of
            "/metrics"     -> doStats mng
            "/stats"       -> doStats mng
            "/wstats"      -> doWStats mng
            "/reopen-log"  -> reopenLog mng $> ok
            "/reload"      -> reloadCmd mng Reload    failed ok
            "/keep-cache"  -> reloadCmd mng KeepCache failed ok
            "/quit"        -> quitCmd mng Quit $> ok
            "/help"        -> doHelp
            "/"            -> doHelp
            _ -> return $ ng HTTP.badRequest400
        | otherwise = return $ ng HTTP.methodNotAllowed405
{- FOURMOLU_ENABLE -}

ok :: Response
ok = responseLBS HTTP.ok200 [] "OK\n"

failed :: Response
failed = responseLBS HTTP.ok200 [] "FAILED\n"

ng :: HTTP.Status -> Response
ng st = responseLBS st [] "NG\n"

{- FOURMOLU_DISABLE -}
bindAPIs :: Config -> IO [Socket]
bindAPIs Config{..}
    | cnf_webapi  = ainfosSkipError putStrLn Stream cnf_webapi_port cnf_webapi_addrs >>= mapM open
    | otherwise   = return []
  where
    open ai@AddrInfo{addrAddress = a} = E.bracketOnError (openSocket ai) close $ \sock -> do
        withLocationIOE (show a ++ "/webapi") $ do
            setSocketOption sock ReuseAddr 1
            when (addrFamily ai == AF_INET6) $ setSocketOption sock IPv6Only 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock a
            listen sock 32
        return sock
{- FOURMOLU_ENABLE -}

-- | Running the web API, handing the action which stops it to the
--   installer.  Requests in progress are given a second to finish.
run :: Control -> (IO () -> IO ()) -> Socket -> IO ()
run mng install sock = E.finally (runSettingsSocket settings sock $ app mng) (close sock)
  where
    settings = setInstallShutdownHandler install $ setGracefulShutdownTimeout (Just 1) defaultSettings

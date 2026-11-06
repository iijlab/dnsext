{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.HTTP3 (
    http3Servers,
) where

-- GHC packages
import Control.Monad (when)
import Data.Functor

-- dnsext-utils
import DNS.Transport.Types

-- other packages
import qualified Network.HTTP3.Server as H3
import qualified Network.QUIC.Server as QUIC
import qualified System.TimeManager as T

-- this package
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Server.Pipeline (extendSynthesis)
import DNS.Iterative.Server.HTTP2
import DNS.Iterative.Server.QUIC
import DNS.Iterative.Server.Stop
import DNS.Iterative.Server.Types
import DNS.Iterative.Server.UDP
import DNS.Iterative.Stats (incStatsDoH3, sessionStatsDoH3)

----------------------------------------------------------------
http3Servers :: VcServerConfig -> ServerActions
http3Servers VcServerConfig{..} synth env toCacher ss = do
    -- fixme: withLocationIOE naming
    when vc_interface_automatic $ mapM_ setPktInfo ss
    name <- mapM socketName ss <&> \xs -> show xs ++ "/h3"
    let http3server = withLocationIOE name $ do
            sessions <- newSessions
            stoppableDraining vc_stop (vc_drain ss) sessions $ \draining install ->
                T.withManager (vc_idle_timeout * 1000000) $ \mgr ->
                QUIC.runWithSockets ss sconf{QUIC.scInstallShutdownHandler = install} $ \conn ->
                    -- Draining sends GOAWAY and ends once the requests taken are answered.
                    withSession sessions $ H3.runIOWith draining conn (conf mgr)
                    $ doHTTP name sbracket incQuery env toCacher H3 synth foldCached foldIterative
    return [http3server]
  where
    (_synth, foldCached, foldIterative) = extendSynthesis synth
    sbracket = sessionStatsDoH3 (stats_ env)
    incQuery inet6 = incStatsDoH3 inet6 (stats_ env)
    sconf = getServerConfig vc_credentials vc_session_manager "h3" (vc_idle_timeout * 1000) env
    conf mgr = H3.defaultConfig{H3.confTimeoutManager = mgr}

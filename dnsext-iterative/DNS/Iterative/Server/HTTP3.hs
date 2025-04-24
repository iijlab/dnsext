{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.HTTP3 (
    http3Servers,
) where

-- GHC packages
import Control.Monad (when)
import Data.Functor

-- dnsext-* packages

-- other packages

import qualified Network.HTTP3.Server as H3
import qualified Network.QUIC.Server as QUIC
import qualified System.TimeManager as T

-- this package
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Server.HTTP2
import DNS.Iterative.Server.QUIC
import DNS.Iterative.Server.Types
import DNS.Iterative.Server.UDP
import DNS.Iterative.Stats (incStatsDoH3, sessionStatsDoH3)

----------------------------------------------------------------
http3Servers :: VcServerConfig -> ServerActions
http3Servers VcServerConfig{..} env toCacher ss = do
    -- fixme: withLocationIOE naming
    when vc_interface_automatic $ mapM_ setPktInfo ss
    name <- mapM socketName ss <&> \xs -> show xs ++ "/h3"
    let http3server = T.withManager (vc_idle_timeout * 1000000) $ \mgr ->
            withLocationIOE name $ QUIC.runWithSockets ss sconf $ \conn ->
                H3.runIO conn (conf mgr) $ doHTTP name sbracket incQuery env toCacher
    return [http3server]
  where
    sbracket = sessionStatsDoH3 (stats_ env)
    incQuery inet6 = incStatsDoH3 inet6 (stats_ env)
    sconf = getServerConfig vc_credentials vc_session_manager "h3" (vc_idle_timeout * 1000)
    conf mgr =
        H3.Config
            { confHooks = H3.defaultHooks
            , confTimeoutManager = mgr
            , confPositionReadMaker = H3.defaultPositionReadMaker
            }

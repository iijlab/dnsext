{-# LANGUAGE OverloadedStrings #-}

module NorecSpec (spec) where

import Control.Concurrent.STM (retry)
import qualified Control.Exception as E
import DNS.Do53.Client (defaultResolveActions)
import DNS.Do53.Internal (ResolveActions (..), ResolveInfo (..), defaultResolveInfo)
import DNS.Iterative.Internal (raceResolve)
import DNS.Types
import Network.Socket
import Test.Hspec

spec :: Spec
spec = describe "raceResolve" $
    -- Thrown, the timeout reached the top of a thread of 'steppedWait'.
    it "answers a single server's timeout as data" $
        E.bracket silentServer close $ \s -> do
            port <- socketPort s
            let ri =
                    defaultResolveInfo
                        { rinfoIP = "127.0.0.1"
                        , rinfoPort = port
                        , rinfoUDPRetry = 1
                        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 100000}
                        }
            r <- E.try $ raceResolve retry [ri] (Question "example.org." A IN) mempty
            let got = case r of
                    Left e -> Left $ show (e :: E.SomeException)
                    Right (Left e) -> Right $ Just $ fst $ unwrapDNSErrorInfo e
                    Right (Right _) -> Right Nothing
            got `shouldBe` Right (Just TimeoutExpired)
  where
    silentServer = do
        s <- socket AF_INET Datagram defaultProtocol
        bind s $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        return s

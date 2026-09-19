{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module IOSpec where

import Control.Exception (throwIO)
import DNS.Do53.Internal
import DNS.Types
import Data.List.NonEmpty (NonEmpty (..))
import Network.Socket (PortNumber)
import Test.Hspec

import FakeServer (withAnswering, withSilent)

q :: Question
q = Question "www.mew.org" A IN

-- | A server on the loopback which answers, and one which does not.
--   Both used to be somebody else's: the answering ones were 8.8.8.8
--   and 1.1.1.1, so the suite needed the internet, and the silent ones
--   were 192.0.2.1 and 192.0.2.2, so it needed those to be a black
--   hole rather than an error.
good :: PortNumber -> ResolveInfo
good port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoUDPRetry = 1
        , rinfoVCLimit = 8 * 1024
        }

quiet :: PortNumber -> ResolveInfo
quiet port =
    defaultResolveInfo
        { rinfoIP = "127.0.0.1"
        , rinfoPort = port
        , rinfoActions = defaultResolveActions{ractionTimeoutTime = 100000}
        , rinfoUDPRetry = 1
        , rinfoVCLimit = 8 * 1024
        }

spec :: Spec
spec = describe "solvers" $ do
    it "resolves well with UDP" $ withAnswering $ \port -> do
        r <- udpResolver (good port) q mempty
        checkNoErr r

    it "resolves well with TCP" $ withAnswering $ \port -> do
        r <- tcpResolver (good port) q mempty
        checkNoErr r

    it "resolves well concurrently (0)" $ withAnswering $ \p0 -> withAnswering $ \p1 -> do
        let renv = ResolveEnv udpResolver True $ good p0 :| [good p1]
        r <- resolve renv q mempty
        checkNoErr r

    it "resolves well concurrently (1)" $ withAnswering $ \p0 -> withSilent $ \p1 -> do
        let renv = ResolveEnv udpResolver True $ good p0 :| [quiet p1]
        r <- resolve renv q mempty
        checkNoErr r

    -- Nobody answers, so every server runs out of tries.  This used to
    -- depend on 192.0.2.1 and 192.0.2.2 swallowing a datagram; where
    -- the kernel answers "network is unreachable" for them instead, it
    -- got a NetworkFailure and failed.
    it "resolves well concurrently (2)" $ withSilent $ \p0 -> withSilent $ \p1 -> do
        let renv = ResolveEnv udpResolver True $ quiet p0 :| [quiet p1]
        r <- resolve renv q mempty
        either (Left . fst . unwrapDNSErrorInfo) Right r `shouldBe` Left RetryLimitExceeded

checkNoErr :: Either DNSError Reply -> Expectation
checkNoErr (Left e) = throwIO e
checkNoErr (Right Reply{..}) = do
    rcode replyDNSMessage `shouldBe` NoErr
    answer replyDNSMessage `shouldNotBe` []

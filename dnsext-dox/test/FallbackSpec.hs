{-# LANGUAGE OverloadedStrings #-}

module FallbackSpec where

import DNS.Do53.Internal
import DNS.DoX.Client
import DNS.SVCB (ALPN)
import DNS.Types
import Data.IORef
import Data.IP (IP)
import qualified Data.List.NonEmpty as NE
import Test.Hspec

spec :: Spec
spec = do
    describe "which designated resolvers a lookup would try" $ do
        -- The records come back sorted by priority and each offers the
        -- protocols it would rather be spoken to in.  All of them are
        -- on offer, so all of them are worth having: the list is what
        -- there is to fall back through.
        it "offers every record, in the order the records asked for" $
            ips (svcbResolveEnvs [[info "dot" "192.0.2.1"], [info "dot" "192.0.2.2"]])
                `shouldBe` [["192.0.2.1"], ["192.0.2.2"]]

        -- A record which offers only a protocol this build cannot speak
        -- turns into no resolver at all.  That is a reason to go on to
        -- the next record, not a reason to give up: the server said
        -- what it would rather have and also what it would settle for.
        it "goes past a record it has no way of speaking to" $
            ips (svcbResolveEnvs [[info "smoke-signal" "192.0.2.1"], [info "dot" "192.0.2.2"]])
                `shouldBe` [["192.0.2.2"]]

    describe "asking each designated resolver in turn" $ do
        -- Nothing to ask is the one case with no answer to give.
        it "has no answer when there is nobody to ask" $ do
            er <- firstToAnswer [] theQuestion mempty
            er `shouldSatisfy` isLeft

        -- The first designated resolver being unreachable is what the
        -- rest of the list is for.  RFC 9460 Sec 2.4.2 has a client
        -- work down the records it was given.
        it "asks the next one when the first does not answer" $ do
            (env0, seen0) <- silent "192.0.2.1"
            (env1, seen1) <- answering "192.0.2.2"
            er <- firstToAnswer [env0, env1] theQuestion mempty
            fmap (nameTagIP . replyTag) er `shouldBe` Right "192.0.2.2"
            readIORef seen0 `shouldReturn` (1 :: Int)
            readIORef seen1 `shouldReturn` 1

        -- And it stops at the one which answered: the others are a
        -- fallback, not a fan-out.
        it "asks no further once one has answered" $ do
            (env0, seen0) <- answering "192.0.2.1"
            (env1, seen1) <- answering "192.0.2.2"
            er <- firstToAnswer [env0, env1] theQuestion mempty
            fmap (nameTagIP . replyTag) er `shouldBe` Right "192.0.2.1"
            readIORef seen0 `shouldReturn` (1 :: Int)
            readIORef seen1 `shouldReturn` 0

theQuestion :: Question
theQuestion = Question "www.example." A IN

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | What one SVCB record offering one protocol turns into.
info :: ALPN -> IP -> SVCBInfo
info alpn ip =
    SVCBInfo
        { svcbInfoALPN = alpn
        , svcbInfoNameTag = tag ip
        , svcbInfoResolveInfos = [at ip]
        }

at :: IP -> ResolveInfo
at ip = defaultResolveInfo{rinfoIP = ip}

tag :: IP -> NameTag
tag ip = nameTag (at ip) "dot"

-- | The addresses of each resolver on offer, kept grouped.
ips :: [ResolveEnv] -> [[IP]]
ips envs = [map rinfoIP $ NE.toList $ renvResolveInfos e | e <- envs]

-- | A resolver which is asked and says nothing, and a count of how
--   often it was asked.
silent :: IP -> IO (ResolveEnv, IORef Int)
silent = fake $ \_ -> pure $ Left TimeoutExpired

-- | A resolver which answers, and a count of how often it was asked.
answering :: IP -> IO (ResolveEnv, IORef Int)
answering = fake $ \ri -> pure $ Right $ reply ri

fake :: (ResolveInfo -> IO (Either DNSError Reply)) -> IP -> IO (ResolveEnv, IORef Int)
fake act ip = do
    ref <- newIORef 0
    let resolver ri _ _ = do
            atomicModifyIORef' ref $ \n -> (n + 1, ())
            act ri
    pure (ResolveEnv resolver True (NE.fromList [at ip]), ref)

reply :: ResolveInfo -> Reply
reply ri =
    Reply
        { replyTag = nameTag ri "dot"
        , replyDNSMessage = defaultQuery
        , replyTxBytes = 0
        , replyRxBytes = 0
        }

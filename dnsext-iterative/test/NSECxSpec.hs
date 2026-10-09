{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NSECxSpec where

import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.List (isInfixOf)
import Test.Hspec

import DNS.RRCache (Ranking (RankAuthAnswer))
import DNS.SEC
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import DNS.Iterative.Internal (
    Env,
    MonadEnv (..),
    newTestEnv,
    noopWorkerStat,
    nsec3WithValid,
 )

instance MonadEnv (ReaderT Env IO) where
    asksEnv = asks
    asksWS f = pure $ f noopWorkerStat

spec :: Spec
spec = do
    runIO $ runInitIO addResourceDataForDNSSEC

    -- A section which holds one NSEC3 record twice is no canonical
    -- RRset, and the RRSIG over it is there.
    describe "an NSEC3 proof read out of a section" $
        it "fails over the RRset, not over a missing RRSIG" $ do
            e <- nsec3Section [nsec3 "h1.example.", sigNSEC3 "h1.example.", nsec3 "h1.example."]
            e `shouldSatisfy` isInfixOf "unique RData"

{- the reason nsec3WithValid gives for refusing a section -}
nsec3Section :: [ResourceRecord] -> IO String
nsec3Section rrs = do
    env <- newTestEnv (const $ pure ()) False 2048
    runReaderT (nsec3WithValid [] id (rrs, RankAuthAnswer) (pure "no NSEC3") pure valid) env
  where
    valid _ _ _ = pure "valid"

nsec3 :: Domain -> ResourceRecord
nsec3 name =
    ResourceRecord name NSEC3 IN 3600 $
        rd_nsec3 Hash_SHA1 [] 0 (Opaque.fromByteString "") (Opaque.fromByteString "next") [A]

sigNSEC3 :: Domain -> ResourceRecord
sigNSEC3 name =
    ResourceRecord name RRSIG IN 3600 $
        rd_rrsig NSEC3 ED25519 2 3600 1893456000 1577836800 12345 "example." (Opaque.fromByteString "")

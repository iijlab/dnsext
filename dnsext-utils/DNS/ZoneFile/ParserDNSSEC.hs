{-# LANGUAGE FlexibleContexts #-}

module DNS.ZoneFile.ParserDNSSEC where

-- GHC packages
import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)

-- dnsext-* packages
import DNS.SEC
import DNS.Types (Opaque, RData)
import qualified DNS.Types.Opaque as Opaque

-- this package
import DNS.Parser
import DNS.ZoneFile.ParserBase
import DNS.ZoneFile.Types

{- FOURMOLU_DISABLE -}
rdatasDNSSEC :: MonadParser Token s m => [(TYPE, m RData)]
rdatasDNSSEC =
    [ (DS     , rdataDS)
    , (DNSKEY , rdataDNSKEY)
    ]
{- FOURMOLU_ENABLE -}

-----

{- FOURMOLU_DISABLE -}
rdataDS :: MonadParser Token s m => m RData
rdataDS =
    rd_ds
    <$> keytag <*> (blank *> pubalg) <*> (blank *> digestalg)
    <*> (blank *> digest)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
rdataDNSKEY :: MonadParser Token s m => m RData
rdataDNSKEY = do
    mkRD  <- rd_dnskey <$> keyflags <*> (blank *> proto)
    alg   <- blank *> pubalg
    pkey  <- blank *> (toPubKey <$> keyB64)
    pure $ mkRD alg pkey
  where
    keyflags = toDNSKEYflags <$> readCString "dnskey.flags"
    proto = readCString "dnskey.proto"
    handleB64 = either (raise . ("Parser.rdataDNSKEY: fromBase64: " ++)) pure
    keyB64 = handleB64 . Opaque.fromBase64 =<< parts
{- FOURMOLU_ENABLE -}

-----

part :: MonadParser Token s m => m ByteString
part = fromShort <$> lstring

parts :: MonadParser Token s m => m ByteString
parts = (mconcat <$>) $ (:) <$> part <*> many (blank *> part)

keytag :: MonadParser Token s m => m KeyTag
keytag = readCString "keytag"

pubalg :: MonadParser Token s m => m PubAlg
pubalg = toPubAlg <$> readCString "pubalg"

digestalg :: MonadParser Token s m => m DigestAlg
digestalg = toDigestAlg <$> readCString "digestalg"

digest :: MonadParser Token s m => m Opaque
digest = handleB16 . Opaque.fromBase16 =<< parts
  where
    handleB16 = either (raise . ("Parser.digest: fromBase16: " ++)) pure

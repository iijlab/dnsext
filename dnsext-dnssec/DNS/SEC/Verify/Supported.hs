{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.Supported (
    getRRSIGImpl,
    getDSImpl,
    getNSEC3Impl,
    supportedDNSKEY,
    supportedRRSIG,
    supportedDS,
) where

-- GHC packages
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- dnsext-types

-- this package
import DNS.SEC.Flags (DNSKEY_Flag (REVOKE, ZONE))
import DNS.SEC.HashAlg
import DNS.SEC.PubAlg
import DNS.SEC.Types
import qualified DNS.SEC.Verify.ECDSA as V
import qualified DNS.SEC.Verify.EdDSA as V
import qualified DNS.SEC.Verify.N3SHA as NSEC3
import qualified DNS.SEC.Verify.RSA as V
import qualified DNS.SEC.Verify.SHA as DS
import DNS.SEC.Verify.Types

----------------------------------------------------------------

getDSImpl :: DigestAlg -> Maybe DSImpl
getDSImpl alg = Map.lookup alg dsDicts

dsDicts :: Map DigestAlg DSImpl
dsDicts =
    Map.fromList
        [ (SHA1, DS.sha1)
        , (SHA256, DS.sha256)
        , (SHA384, DS.sha384)
        ]

----------------------------------------------------------------

getRRSIGImpl :: PubAlg -> Maybe RRSIGImpl
getRRSIGImpl alg = Map.lookup alg pubkeyDicts

{- FOURMOLU_DISABLE -}
pubkeyDicts :: Map PubAlg RRSIGImpl
pubkeyDicts =
    Map.fromList
        [ (RSASHA1,             V.rsaSHA1)
        , (RSASHA1_NSEC3_SHA1,  V.rsaSHA1)  {- https://datatracker.ietf.org/doc/html/rfc5155#section-2 -}
        , (RSASHA256,           V.rsaSHA256)
        , (RSASHA512,           V.rsaSHA512)
        , (ECDSAP256SHA256,     V.ecdsaP256SHA)
        , (ECDSAP384SHA384,     V.ecdsaP384SHA)
        , (ED25519,             V.ed25519)
        , (ED448,               V.ed448)
        ]
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

getNSEC3Impl :: HashAlg -> Maybe NSEC3Impl
getNSEC3Impl alg = Map.lookup alg nsec3Dicts

nsec3Dicts :: Map HashAlg NSEC3Impl
nsec3Dicts =
    Map.fromList
        [ (Hash_SHA1, NSEC3.n3sha1)
        ]

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
supportedDNSKEY :: RD_DNSKEY -> Bool
supportedDNSKEY RD_DNSKEY{..} =
   ZONE `elem` dnskey_flags       &&  {- https://datatracker.ietf.org/doc/html/rfc4034#section-2.1.1 -}
   REVOKE `notElem` dnskey_flags  &&  {- https://datatracker.ietf.org/doc/html/rfc5011#section-2.1 -}
   dnskey_protocol == 3           &&  {- https://datatracker.ietf.org/doc/html/rfc4034#section-2.1.2 -}
   Map.member dnskey_pubalg pubkeyDicts
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

supportedRRSIG :: RD_RRSIG -> Bool
supportedRRSIG RD_RRSIG{..} = Map.member rrsig_pubalg pubkeyDicts

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
supportedDS :: RD_DS -> Bool
supportedDS RD_DS{..} =
    Map.member ds_digestalg dsDicts  &&
    Map.member ds_pubalg pubkeyDicts
{- FOURMOLU_ENABLE -}

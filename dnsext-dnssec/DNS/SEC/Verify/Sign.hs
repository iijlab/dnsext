{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.Sign (
    -- * Sign
    sign,
    genKeyPair,
    makeDNSKEY,
    makeDS,
)
where

import DNS.SEC
import DNS.SEC.Verify.RRset
import DNS.SEC.Verify.Supported (getDSImpl, getRRSIGImpl)
import DNS.SEC.Verify.Types
import DNS.SEC.Verify.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

import Data.Maybe

sign
    :: PriKey -> [ResourceRecord] -> RD_RRSIG -> IO (Either String RD_RRSIG)
sign pri rrs rrsig = case getRRSIGImpl alg of
    Nothing -> return $ Left $ show alg
    Just impl -> do
        esig <- doSign impl pri rrs rrsig
        case esig of
            Left s -> return $ Left s
            Right sig -> return $ Right $ rrsig{rrsig_signature = sig}
  where
    alg = rrsig_pubalg rrsig

doSign
    :: RRSIGImpl
    -> PriKey
    -> [ResourceRecord]
    -> RD_RRSIG
    -> IO (Either String Opaque)
doSign RRSIGImpl{..} pri rrs rrsig = do
    case rrsigIDecodePriKey pri of
        Left e -> return $ Left $ show e
        Right priK -> do
            let (sortedRDatas, sortedRRs) = unzip $ sortRDataCanonical rrs
            canonicalRRsetSorted sortedRRs (return . Left) $
                \rrset_dom typ cls _ttl _rds -> do
                    let str = encodeRRset rrsig rrset_dom typ cls sortedRDatas
                    Right . rrsigIEncodeSignature <$> rrsigISign priK str

genKeyPair :: PubAlg -> IO (Maybe (PubKey, PriKey))
genKeyPair alg = case getRRSIGImpl alg of
    Nothing -> return Nothing
    Just RRSIGImpl{..} -> do
        (pub, pri) <- rrsigIGenKeyPair
        let pubkey = rrsigIEncodePubKey pub
            prikey = rrsigIEncodePriKey pri
        return $ Just (pubkey, prikey)

makeDNSKEY :: PubAlg -> PubKey -> Bool -> RD_DNSKEY
makeDNSKEY alg pub ksk =
    RD_DNSKEY
        { dnskey_flags = [ZONE] ++ if ksk then [SecureEntryPoint] else []
        , dnskey_protocol = 3
        , dnskey_pubalg = alg
        , dnskey_public_key = pub
        }

makeDS :: Domain -> DigestAlg -> RD_DNSKEY -> RD_DS
makeDS owner digestalg dnskey =
    RD_DS
        { ds_key_tag = tag
        , ds_pubalg = dnskey_pubalg dnskey
        , ds_digestalg = digestalg
        , ds_digest = Opaque.fromByteString $ calcDigest dsimpl dnskey owner
        }
  where
    tag = keyTag dnskey
    dsimpl = fromJust $ getDSImpl digestalg

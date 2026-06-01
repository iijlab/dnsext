{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.Sign (
    -- * Sign
    sign,
    genKeyPair,
)
where

import DNS.SEC
import DNS.SEC.Verify.RRset
import DNS.SEC.Verify.Supported (getRRSIGImpl)
import DNS.SEC.Verify.Types
import DNS.Types

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

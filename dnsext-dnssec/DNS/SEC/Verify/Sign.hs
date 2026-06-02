{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.Sign (
    -- * Sign
    sign,
    sign', -- for testing
    genKeyPair,
    makeDNSKEY,
    makeDS,
    signZone,
)
where

import DNS.SEC
import DNS.SEC.Verify.RRset
import DNS.SEC.Verify.Supported (getDSImpl, getRRSIGImpl)
import DNS.SEC.Verify.Types
import DNS.SEC.Verify.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque
import DNS.Types.Time

import Control.Exception as E
import Data.ByteString ()
import Data.List
import Data.Maybe

data SignFailure = SignFailure deriving (Show)

instance Exception SignFailure

sign :: PriKey -> RD_RRSIG -> [ResourceRecord] -> IO ResourceRecord
sign _ _ [] = E.throwIO SignFailure
sign pri rrsig rrs@(rr : _) = do
    rrsig' <- sign' pri rrsig rrs
    let rd = toRData rrsig'
    return $ rr{rrtype = RRSIG, rdata = rd}

sign' :: PriKey -> RD_RRSIG -> [ResourceRecord] -> IO RD_RRSIG
sign' pri rrsig rrs = case getRRSIGImpl alg of
    Nothing -> E.throwIO SignFailure
    Just impl -> do
        sig <- doSign impl pri rrs rrsig
        return rrsig{rrsig_signature = sig}
  where
    alg = rrsig_pubalg rrsig

doSign
    :: RRSIGImpl
    -> PriKey
    -> [ResourceRecord]
    -> RD_RRSIG
    -> IO Opaque
doSign RRSIGImpl{..} pri rrs rrsig = do
    case rrsigIDecodePriKey pri of
        Left _ -> E.throwIO SignFailure
        Right priK -> do
            let (sortedRDatas, sortedRRs) = unzip $ sortRDataCanonical rrs
            canonicalRRsetSorted sortedRRs (\_ -> E.throwIO SignFailure) $
                \rrset_dom typ cls _ttl _rds -> do
                    let str = encodeRRset rrsig rrset_dom typ cls sortedRDatas
                    rrsigIEncodeSignature <$> rrsigISign priK str

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

signZone :: Domain -> PubAlg -> [ResourceRecord] -> IO [ResourceRecord]
signZone zone alg rrs0 = E.handle handler $ do
    mp <- genKeyPair alg
    case mp of
        Nothing -> E.throwIO SignFailure
        Just (pubkey, prikey) -> do
            let dnskey = makeDNSKEY alg pubkey False
                tag = keyTag dnskey
                rrdnskey =
                    ResourceRecord
                        { rrname = zone
                        , rrtype = DNSKEY
                        , rrclass = IN
                        , rrttl = 3600 -- fixme
                        , rdata = toRData dnskey
                        }
            inception <- toDNSTime <$> getCurrentTime
            let expiration = inception + 86400 -- fixme
            mapM (f prikey tag inception expiration) ([rrdnskey] : rrss)
  where
    handler SignFailure = return []
    sortedRRs = sort rrs0
    rreq r0 r1 =
        rrname r0 == rrname r1
            && rrtype r0 == rrtype r1
            && rrclass r0 == rrclass r1
            && rrttl r0 == rrttl r1
    rrss = groupBy rreq sortedRRs
    f _ _ _ _ [] = E.throwIO SignFailure
    f prikey tag inception expiration rrs@(ResourceRecord{..} : _) = sign prikey rrsig rrs
      where
        rrsig =
            RD_RRSIG
                { rrsig_type = rrtype
                , rrsig_pubalg = alg
                , rrsig_num_labels = fromIntegral $ labelsCount rrname
                , rrsig_ttl = rrttl
                , rrsig_expiration = expiration
                , rrsig_inception = inception
                , rrsig_key_tag = tag
                , rrsig_zone = zone
                , rrsig_signature = Opaque.fromByteString ""
                }

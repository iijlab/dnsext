{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.Sign (
    -- * Sign
    sign,
    sign', -- for testing
    genKeyPair,
    makeDNSKEY,
    makeDS,
    DNSSECinfo (..),
    prepareDNSSEC,
    RRSetSig (..),
    groupRRset,
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
import Data.Word (Word16)

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

data DNSSECinfo = DNSSECinfo
    { dnssecInfoZone :: Domain
    , dnssecInfoPubAlg :: PubAlg
    , dnssecInfoDigestAlg :: DigestAlg
    , dnssecInfoTTL :: TTL
    -- ^ TTL for DNSKEY and DS
    , dnssecInfoDuration :: DNSTime
    -- ^ Duration of RRSIG. This value is added to inception to
    -- calculate expiration.
    }

data RRSetSig = RRSetSig
    { rrsetsigName :: Domain
    , rrsetsigType :: TYPE
    , rrsetsigRRs :: [ResourceRecord]
    , rrsetsigSig :: Maybe ResourceRecord
    }
    deriving (Show)

prepareDNSSEC
    :: DNSSECinfo
    -> IO
        ( PubKey
        , PriKey
        , ResourceRecord -- DNSKEY
        , ResourceRecord -- DS
        , Bool -> [ResourceRecord] -> IO [RRSetSig]
        )
prepareDNSSEC info@DNSSECinfo{..} = do
    mp <- genKeyPair dnssecInfoPubAlg
    case mp of
        Nothing -> E.throwIO SignFailure
        Just (pubkey, prikey) -> do
            let dnskey = makeDNSKEY dnssecInfoPubAlg pubkey True -- fixme
                ds = makeDS dnssecInfoZone dnssecInfoDigestAlg dnskey
                tag = ds_key_tag ds
                rrdnskey =
                    ResourceRecord
                        { rrname = dnssecInfoZone
                        , rrtype = DNSKEY
                        , rrclass = IN
                        , rrttl = dnssecInfoTTL
                        , rdata = toRData dnskey
                        }
                rrds =
                    ResourceRecord
                        { rrname = dnssecInfoZone
                        , rrtype = DS
                        , rrclass = IN
                        , rrttl = dnssecInfoTTL
                        , rdata = toRData ds
                        }
            rrsigTemp <- makeRRSIGtemplate info tag
            let signRRs = signZone prikey rrsigTemp
            return (pubkey, prikey, rrdnskey, rrds, signRRs)

makeRRSIGtemplate :: DNSSECinfo -> Word16 -> IO RD_RRSIG
makeRRSIGtemplate DNSSECinfo{..} tag = do
    inception <- toDNSTime <$> getCurrentTime
    let expiration = inception + dnssecInfoDuration
    return $
        RD_RRSIG
            { rrsig_type = A -- overridden
            , rrsig_pubalg = dnssecInfoPubAlg
            , rrsig_num_labels = 0 -- overridden
            , rrsig_ttl = 0 -- overridden
            , rrsig_expiration = expiration
            , rrsig_inception = inception
            , rrsig_key_tag = tag
            , rrsig_zone = dnssecInfoZone
            , rrsig_signature = Opaque.fromByteString "" -- overridden
            }

groupRRset :: [ResourceRecord] -> [[ResourceRecord]]
groupRRset rrs = groupBy rreq $ sort rrs
  where
    rreq r0 r1 =
        rrname r0 == rrname r1
            && rrtype r0 == rrtype r1
            && rrclass r0 == rrclass r1
            && rrttl r0 == rrttl r1

signZone
    :: PriKey
    -> RD_RRSIG
    -> Bool
    -> [ResourceRecord]
    -> IO [RRSetSig]
signZone prikey rrsigTemp0 groupup rrs0 = E.handle handler $ mapM f rrss
  where
    handler SignFailure = return []
    rrss
        | groupup = groupRRset rrs0
        | otherwise = map (: []) rrs0
    f [] = E.throwIO SignFailure
    f rrs@(ResourceRecord{..} : _) = do
        sig <- sign prikey rrsigTemp rrs
        return $
            RRSetSig
                { rrsetsigName = rrname
                , rrsetsigType = rrtype
                , rrsetsigRRs = rrs
                , rrsetsigSig = Just sig
                }
      where
        rrsigTemp =
            rrsigTemp0
                { rrsig_type = rrtype
                , rrsig_num_labels = fromIntegral $ labelsCount rrname
                , rrsig_ttl = rrttl
                }

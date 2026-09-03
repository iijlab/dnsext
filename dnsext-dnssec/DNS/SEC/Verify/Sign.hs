{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.SEC.Verify.Sign (
    -- * Sign
    sign,
    sign', -- for testing
    genKeyPair,
    makeDNSKEY,
    makeDS,
    KeyConfig (..),
    KeyInfo (..),
    generateKeyInfo,
    toKeyInfo,
    fromKeyInfo,
    makeSigner,
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

import Control.Exception (Exception)
import qualified Control.Exception as E
import Data.ByteString ()
import Data.List
import Data.Maybe
import Data.Word

----------------------------------------------------------------

data KeyConfig = KeyConfig
    { keyConfZone :: Domain
    , keyConfPubAlg :: PubAlg
    , keyConfDigestAlg :: DigestAlg
    , keyConfTTL :: TTL
    -- ^ TTL for DNSKEY and DS
    , keyConfDuration :: DNSTime
    -- ^ Duration of RRSIG. This value is added to inception to
    -- calculate expiration.
    }
    deriving (Eq, Show)

data KeyInfo = KeyInfo
    { keyInfoZone :: Domain
    , keyInfoAlgorithm :: PubAlg
    , keyInfoDigestAlgo :: DigestAlg
    , keyInfoTag :: KeyTag
    , keyInfoDigest :: Opaque
    , keyInfoPubKey :: PubKey
    , keyInfoPriKey :: PriKey
    , keyInfoFlag :: Word16
    }
    deriving (Eq, Show)

data RRSetSig = RRSetSig
    { rrsetsigName :: Domain
    , rrsetsigType :: TYPE
    , rrsetsigRRs :: [ResourceRecord]
    , rrsetsigSig :: Maybe ResourceRecord
    }
    deriving (Show, Eq, Ord)

----------------------------------------------------------------

data SignFailure = SignFailure deriving (Show)

instance Exception SignFailure

----------------------------------------------------------------

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

----------------------------------------------------------------

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

----------------------------------------------------------------

generateKeyInfo
    :: KeyConfig
    -> IO
        ( KeyInfo
        , ResourceRecord -- DNSKEY
        , ResourceRecord -- DS
        )
generateKeyInfo KeyConfig{..} = do
    mp <- genKeyPair keyConfPubAlg
    case mp of
        Nothing -> E.throwIO SignFailure
        Just (pubkey, prikey) -> do
            let dnskey = makeDNSKEY keyConfPubAlg pubkey True -- fixme
                ds = makeDS keyConfZone keyConfDigestAlg dnskey
                keyInfo = toKeyInfo keyConfZone prikey dnskey ds
                (rrdnskey, rrds) = toRRs keyConfZone keyConfTTL dnskey ds
            return (keyInfo, rrdnskey, rrds)

toRRs :: Domain -> TTL -> RD_DNSKEY -> RD_DS -> (ResourceRecord, ResourceRecord)
toRRs zone ttl dnskey ds = (rrdnskey, rrds)
  where
    rrdnskey =
        ResourceRecord
            { rrname = zone
            , rrtype = DNSKEY
            , rrclass = IN
            , rrttl = ttl
            , rdata = toRData dnskey
            }
    rrds =
        ResourceRecord
            { rrname = zone
            , rrtype = DS
            , rrclass = IN
            , rrttl = ttl
            , rdata = toRData ds
            }

toKeyInfo :: Domain -> PriKey -> RD_DNSKEY -> RD_DS -> KeyInfo
toKeyInfo zone prikey RD_DNSKEY{..} RD_DS{..} =
    KeyInfo
        { keyInfoZone = zone
        , keyInfoAlgorithm = dnskey_pubalg
        , keyInfoDigestAlgo = ds_digestalg
        , keyInfoTag = ds_key_tag
        , keyInfoDigest = ds_digest
        , keyInfoPubKey = dnskey_public_key
        , keyInfoPriKey = prikey
        , keyInfoFlag = fromDNSKEYflags dnskey_flags
        }

fromKeyInfo :: KeyInfo -> TTL -> (RD_DNSKEY, RD_DS, ResourceRecord, ResourceRecord)
fromKeyInfo KeyInfo{..} ttl = (dnskey, ds, rrdnskey, rrds)
  where
    dnskey = makeDNSKEY keyInfoAlgorithm keyInfoPubKey True -- fixme
    ds = makeDS keyInfoZone keyInfoDigestAlgo dnskey
    (rrdnskey, rrds) = toRRs keyInfoZone ttl dnskey ds

makeSigner :: KeyConfig -> KeyInfo -> IO (Bool -> [ResourceRecord] -> IO [RRSetSig])
makeSigner conf KeyInfo{..} = do
    rrsigTemp <- makeRRSIGtemplate conf keyInfoTag
    let signer = signZone keyInfoPriKey rrsigTemp
    return signer

prepareDNSSEC
    :: KeyConfig
    -> IO
        ( PubKey
        , PriKey
        , ResourceRecord -- DNSKEY
        , ResourceRecord -- DS
        , Bool -> [ResourceRecord] -> IO [RRSetSig]
        )
prepareDNSSEC conf@KeyConfig{..} = do
    mp <- genKeyPair keyConfPubAlg
    case mp of
        Nothing -> E.throwIO SignFailure
        Just (pubkey, prikey) -> do
            let dnskey = makeDNSKEY keyConfPubAlg pubkey True -- fixme
                ds = makeDS keyConfZone keyConfDigestAlg dnskey
                tag = ds_key_tag ds
                rrdnskey =
                    ResourceRecord
                        { rrname = keyConfZone
                        , rrtype = DNSKEY
                        , rrclass = IN
                        , rrttl = keyConfTTL
                        , rdata = toRData dnskey
                        }
                rrds =
                    ResourceRecord
                        { rrname = keyConfZone
                        , rrtype = DS
                        , rrclass = IN
                        , rrttl = keyConfTTL
                        , rdata = toRData ds
                        }
            rrsigTemp <- makeRRSIGtemplate conf tag
            let signRRs = signZone prikey rrsigTemp
            return (pubkey, prikey, rrdnskey, rrds, signRRs)

makeRRSIGtemplate :: KeyConfig -> KeyTag -> IO RD_RRSIG
makeRRSIGtemplate KeyConfig{..} tag = do
    inception <- toDNSTime <$> getCurrentTime
    let expiration = inception + keyConfDuration
    return $
        RD_RRSIG
            { rrsig_type = A -- overridden
            , rrsig_pubalg = keyConfPubAlg
            , rrsig_num_labels = 0 -- overridden
            , rrsig_ttl = 0 -- overridden
            , rrsig_expiration = expiration
            , rrsig_inception = inception
            , rrsig_key_tag = tag
            , rrsig_zone = keyConfZone
            , rrsig_signature = Opaque.fromByteString "" -- overridden
            }

----------------------------------------------------------------

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
        labels = case leafDomain rrname of
            Nothing -> 0
            Just l ->
                let n0 = labelsCount rrname
                    n
                        | l == "*" = n0 - 1
                        | otherwise = n0
                 in n
        rrsigTemp =
            rrsigTemp0
                { rrsig_type = rrtype
                , rrsig_num_labels = fromIntegral labels
                , rrsig_ttl = rrttl
                }

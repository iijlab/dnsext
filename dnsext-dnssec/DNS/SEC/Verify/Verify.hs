{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DNS.SEC.Verify.Verify (
    -- * Verify
    verifyRRSIG,
    verifyRRSIGsorted,

    -- * Key Tag
    keyTag,
    checkKeyTag,

    -- * DS
    verifyDS,

    -- * NSEC3
    zipSigsNSEC3,
    nameErrorNSEC3,
    noDataNSEC3,
    unsignedDelegationNSEC3,
    wildcardExpansionNSEC3,
    wildcardNoDataNSEC3,
    detectWildcardExpansionNSEC3,
    hashNSEC3,
    hashNSEC3PARAM,
    detectNSEC3,

    -- * NSEC
    zipSigsNSEC,
    nameErrorNSEC,
    noDataNSEC,
    unsignedDelegationNSEC,
    wildcardExpansionNSEC,
    wildcardNoDataNSEC,
    detectNSEC,

    -- * Misc
    withWildcard,
) where

-- GHC packages
import qualified Data.ByteString.Internal as BS
import Foreign.ForeignPtr (withForeignPtr)
import GHC.Exts hiding (TYPE)
import GHC.ST (ST (..), runST)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- dnsext-types
import DNS.Types
import DNS.Types.Internal
import qualified DNS.Types.Opaque as Opaque

-- this package
import DNS.SEC.Flags (DNSKEY_Flag (REVOKE, ZONE))
import DNS.SEC.Imports
import DNS.SEC.PubAlg
import DNS.SEC.Time (DNSTime)
import DNS.SEC.Types
import qualified DNS.SEC.Verify.NSEC as NSEC
import qualified DNS.SEC.Verify.NSEC3 as NSEC3
import qualified DNS.SEC.Verify.NSECxRange as NRange
import DNS.SEC.Verify.RRset
import DNS.SEC.Verify.Supported
import DNS.SEC.Verify.Types

-- $setup
-- >>> :seti -XOverloadedStrings

-- | Calculating a Key Tag for a DNSKEY.
keyTag :: RD_DNSKEY -> Word16
keyTag dnskey = keyTagFromBS $ runBuilder (resourceDataSize dnskey) $ putResourceData Canonical dnskey

{- FOURMOLU_DISABLE -}
-- | Implementation for the Key Tag algorithm from https://datatracker.ietf.org/doc/html/rfc4034#appendix-B
keyTagFromBS :: ByteString -> Word16
keyTagFromBS (BS.BS ftpr (I# len#)) =
    fromIntegral $ unsafeDupablePerformIO $ withForeignPtr ftpr $ return . go 0 0
  where
    go :: Int -> Word -> Ptr Word8 -> Word
    go (I# i0) (W# ac0) (Ptr ptr0#) = runST $ ST $ loop i0 ac0
      where
        loop :: Int# -> Word# -> State# d -> (# State# d, Word #) {- unboxed-word calculation -}
        loop i0# ac0# s = case len# -# i0# of
            0# -> (# s, final ac0# #)
            1# -> case readWord8OffAddr# ptr0# i0# s of
                (# s, key0# #) ->
                    let ac1# = ac0# `plusWord#` (word8ToWord# key0# `uncheckedShiftL#` 8#)
                     in (# s, final ac1# #)
            _ -> case readWord8OffAddr# ptr0# i0# s of
                (# s, key0# #) -> case readWord8OffAddr# ptr0# (i0# +# 1#) s of
                    (# s, key1# #) ->
                        let ac2# =
                                ac0#
                                    `plusWord#` (word8ToWord# key0# `uncheckedShiftL#` 8#)
                                    `plusWord#` word8ToWord# key1#
                            i2# = i0# +# 2#
                         in loop i2# ac2# s
    final :: Word# -> Word
    final ac# = W# ((ac# `plusWord#` ((ac# `uncheckedShiftRL#` 16#) `and#` 0xFFFF##)) `and#` 0xFFFF##)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | Check if the Key Tag matches the calculated Key Tag of DNSKEY.
checkKeyTag :: RD_DNSKEY -> Word16 -> Either String ()
checkKeyTag dnskey@RD_DNSKEY{..} tag = do
    let keyTag_ = keyTag dnskey
    when (dnskey_pubalg == RSAMD5) $
        Left "checkKeyTag: not implemented key-tag computation for RSAMD5"
    unless (keyTag_ == tag) $
        Left $
            "checkKeyTag: Key Tag mismatch between DNSKEY and RRSIG: "
         ++ show keyTag_
         ++ " =/= "
         ++ show tag
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
verifyRRSIGwith
    :: RRSIGImpl
    -> DNSTime
    -> RD_DNSKEY
    -> RD_RRSIG
    -> Domain
    -> TYPE
    -> CLASS
    -> [(Int, Builder ())]
    -> Either String ()
verifyRRSIGwith RRSIGImpl{..} now RD_DNSKEY{..} rrsig@RD_RRSIG{..} rrset_name rrset_type rrset_class sortedRDatas' = do
    unless (ZONE `elem` dnskey_flags) $
        {- https://datatracker.ietf.org/doc/html/rfc4034#section-2.1.1
           "If bit 7 has value 0, then the DNSKEY record holds some other type of DNS public key
            and MUST NOT be used to verify RRSIGs that cover RRsets." -}
        Left "verifyRRSIGwith: ZONE flag is not set for DNSKEY flags"
    when (REVOKE `elem` dnskey_flags) $
        {- https://datatracker.ietf.org/doc/html/rfc5011#section-2.1
         "Once the resolver sees the REVOKE bit, it MUST NOT use this key as a trust anchor or for any other purpose except
          to validate the RRSIG it signed over the DNSKEY RRSet specifically for the purpose of validating the revocation." -}
        Left "verifyRRSIGwith: REVOKE flag is set for DNSKEY flags"
    unless (dnskey_protocol == 3) $
        {- https://datatracker.ietf.org/doc/html/rfc4034#section-2.1.2  "The Protocol Field MUST have value 3" -}
        Left $
            "verifyRRSIGwith: protocol number of DNSKEY is not 3: " ++ show dnskey_protocol
    unless (dnskey_pubalg == rrsig_pubalg) $
        Left $
            "verifyRRSIGwith: pubkey algorithm mismatch between DNSKEY and RRSIG: "
         ++ show dnskey_pubalg
         ++ " =/= "
         ++ show rrsig_pubalg

    let nlabels = numLabels rrset_name
    unless (nlabels >= fromIntegral rrsig_num_labels) $
        Left $
            "verifyRRSIGwith: number of rrname labels is too small: "
         ++ ("rrname-labels=" ++ show nlabels)
         ++ " < "
         ++ ("rrsig-labels=" ++ show rrsig_num_labels)
    unless (rrsig_inception <= now && now < rrsig_expiration) $
        Left $
            "verifyRRSIGwith: not valid period of RRSIG: to be valid, time "
         ++ show now
         ++ " must be between "
         ++ show rrsig_inception
         ++ " and "
         ++ show rrsig_expiration

    unless (rrset_type == rrsig_type) $
        Left $
            "verifyRRSIGwith: TYPE mismatch between RRset and RRSIG: "
         ++ show rrset_type
         ++ " =/= "
         ++ show rrsig_type

    pubkey <- rrsigIDecodePubKey dnskey_public_key
    sig <- rrsigIDecodeSignature rrsig_signature
    let str = encodeRRset rrsig rrset_name rrset_type rrset_class sortedRDatas'
    {- `Data.List.sort` is linear for sorted case -}
    good <- rrsigIVerify pubkey sig str
    unless good $ Left "verifyRRSIGwith: rejected on verification"
{- FOURMOLU_ENABLE -}

verifyRRSIGsorted
    :: DNSTime
    -- ^ The current time
    -> RD_DNSKEY
    -> RD_RRSIG
    -> Domain
    -- ^ RRSet name
    -> TYPE
    -> CLASS
    -> [(Int, Builder ())]
    -> Either String ()
verifyRRSIGsorted now dnskey rrsig name typ cls sortedRDatas =
    maybe (Left $ "verifyRRSIGsorted: unsupported algorithm: " ++ show alg) verify $ getRRSIGImpl alg
  where
    alg = dnskey_pubalg dnskey
    verify impl = verifyRRSIGwith impl now dnskey rrsig name typ cls sortedRDatas

{- FOURMOLU_DISABLE -}
-- | Verifying the signature (RRSIG) of RRSet using the public key (DNSKEY).
--   RRSet is canonicalized automatically.
verifyRRSIG
    :: DNSTime
    -- ^ The current time
    -> Domain
    -- ^ Zone name
    -> RD_DNSKEY
    -> Domain
    -- ^ RRset name
    -> RD_RRSIG
    -> [ResourceRecord]
    -> Either String ()
verifyRRSIG now zone dnskey owner rrsig@RD_RRSIG{..} rrs = do
    unless (rrsig_zone == zone) $
        Left $
            "verifyRRSIG: RRSIG zone mismatch: "
                ++ show rrsig_zone
                ++ " =/= "
                ++ show zone
    {- The RRset MUST be sorted in canonical order.
       https://datatracker.ietf.org/doc/html/rfc4034#section-3.1.8.1 -}
    let (sortedRDatas, sortedRRs) = unzip $ sortRDataCanonical rrs
    canonicalRRsetSorted sortedRRs Left $
        \rrset_dom typ cls _ttl _rds -> do
            unless (rrset_dom == owner) $
                Left $
                    "verifyRRSIG: RRset domain mismatch with owner-domain: "
                        ++ show rrset_dom
                        ++ " =/= "
                        ++ show owner
            verifyRRSIGsorted now dnskey rrsig rrset_dom typ cls sortedRDatas

{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
verifyDSwith :: DSImpl -> Domain -> RD_DNSKEY -> RD_DS -> Either String ()
verifyDSwith DSImpl{..} owner dnskey@RD_DNSKEY{..} RD_DS{..} = do
    unless (ZONE `elem` dnskey_flags) $
        {- https://datatracker.ietf.org/doc/html/rfc4034#section-5.2
           "The DNSKEY RR referred  to in the DS RR MUST be a DNSSEC zone key." -}
        Left "verifyDSwith: ZONE flag is not set for DNSKEY flags"
    unless (dnskey_pubalg == ds_pubalg) $
        Left $
            "verifyDSwith: pubkey algorithm mismatch between DNSKEY and DS: "
         ++ show dnskey_pubalg
         ++ " =/= "
         ++ show ds_pubalg
    let dnskeyBS = runBuilder (resourceDataSize dnskey) $ putResourceData Canonical dnskey
        digest = dsIGetDigest (runBuilder (domainSize owner) (putDomain Canonical owner) <> dnskeyBS)
        ds_digest' = Opaque.toByteString ds_digest
    unless (dsIVerify digest ds_digest') $
        Left "verifyDSwith: rejected on verification"
{- FOURMOLU_ENABLE -}

verifyDS :: Domain -> RD_DNSKEY -> RD_DS -> Either String ()
verifyDS owner dnskey ds =
    maybe (Left $ "verifyDS: unsupported algorithm: " ++ show alg) verify $
        getDSImpl alg
  where
    alg = ds_digestalg ds
    verify impl = verifyDSwith impl owner dnskey ds

---

hashNSEC3with' :: NSEC3Impl -> Word16 -> Opaque -> Domain -> Opaque
hashNSEC3with' NSEC3Impl{..} iter osalt domain =
    Opaque.fromByteString $ recurse iter
  where
    recurse i
        | i <= 0 = step $ runBuilder (domainSize domain) $ putDomain Canonical domain
        | otherwise = step $ recurse $ i - 1
    step = nsec3IGetBytes . nsec3IGetHash . (<> salt)
    salt = Opaque.toByteString osalt

hashNSEC3with :: NSEC3Impl -> RD_NSEC3 -> Domain -> Opaque
hashNSEC3with impl RD_NSEC3{..} domain =
    hashNSEC3with' impl nsec3_iterations nsec3_salt domain

-- | `nsec3param_flags` should be checked outside.
--   https://datatracker.ietf.org/doc/html/rfc5155#section-4.1.2
--   "NSEC3PARAM RRs with a Flags field value other than zero MUST be ignored."
hashNSEC3PARAMwith :: NSEC3Impl -> RD_NSEC3PARAM -> Domain -> Opaque
hashNSEC3PARAMwith impl RD_NSEC3PARAM{..} domain =
    hashNSEC3with' impl nsec3param_iterations nsec3param_salt domain

---

hashNSEC3 :: RD_NSEC3 -> Domain -> Either String Opaque
hashNSEC3 nsec3 domain =
    maybe (Left $ "hashNSEC3: unsupported algorithm: " ++ show alg) (Right . hash) $
        getNSEC3Impl alg
  where
    alg = nsec3_hashalg nsec3
    hash impl = hashNSEC3with impl nsec3 domain

hashNSEC3PARAM :: RD_NSEC3PARAM -> Domain -> Either String Opaque
hashNSEC3PARAM nsec3p domain =
    maybe
        (Left $ "hashNSEC3PARAM: unsupported algorithm: " ++ show alg)
        (Right . hash)
        $ getNSEC3Impl alg
  where
    alg = nsec3param_hashalg nsec3p
    hash impl = hashNSEC3PARAMwith impl nsec3p domain

---

{- FOURMOLU_DISABLE -}
-- |
-- >>> withWildcard' qn nlabel = withWildcard qn nlabel id "not wildcard" (\wild nc -> show wild ++ " " ++ show nc)
-- >>> withWildcard' "a.example." 2
-- "not wildcard"
-- >>> withWildcard' "a.example." 3
-- "inconsistent nlabel:..."
-- >>> withWildcard' "a.b.w.example." 2
-- "\"*.w.example.\" \"b.w.example.\""
withWildcard :: Domain -> Word8 -> (String -> a) -> a -> (Domain -> Domain -> a) -> a
withWildcard qname nlabel left0 nw wild
    | wmatches <  0  = left "inconsistent nlabel"
    | wmatches == 0  = nw
    | otherwise      = case ncs of
        []          -> left "inconsistent length of wildcard matched"
        _:ws        -> wild (fromWireLabels $ fromString "*" : ws) (fromWireLabels ncs)
  where
    left s = left0 $ s ++ ": nlabel=" ++ show nlabel ++ ", " ++ show qname
    wmatches = labelsCount qname - fromIntegral nlabel
    ncs = drop (wmatches - 1) $ toWireLabels qname :: [Label]
{- FOURMOLU_ENABLE -}

---

zipSigsNSEC3 :: [ResourceRecord] -> (String -> a) -> ([(ResourceRecord, NSEC3_Range, [(RD_RRSIG, TTL)])] -> a) -> a
zipSigsNSEC3 = NRange.zipSigsets NSEC3.rangeImpl

getNSEC3Result :: NSEC3.Logic a -> Domain -> [NSEC3_Range] -> Domain -> Either String a
getNSEC3Result hl zone cs qname =
    withImpls $ \ps -> NSEC3.getResult hl zone [(c, hashNSEC3with impl nsec3) | (impl, c@(_, nsec3)) <- ps] qname
  where
    withImpls h = h =<< mapM addImpl cs
    addImpl r@(_, nsec3) = do
        let alg = nsec3_hashalg nsec3
        impl <- maybe (Left $ "NSEC3: unsupported algorithm: " ++ show alg) Right $ getNSEC3Impl alg
        return (impl, r)

nameErrorNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> Either String NSEC3_NameError
nameErrorNSEC3 = getNSEC3Result NSEC3.get_nameError

noDataNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> TYPE -> Either String NSEC3_NoData
noDataNSEC3 zone ranges qname qtype = getNSEC3Result (NSEC3.get_noData qtype) zone ranges qname

unsignedDelegationNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> Either String NSEC3_UnsignedDelegation
unsignedDelegationNSEC3 = getNSEC3Result NSEC3.get_unsignedDelegation

detectWildcardExpansionNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> Either String NSEC3_WildcardExpansion
detectWildcardExpansionNSEC3 = getNSEC3Result NSEC3.detect_wildcardExpansion

wildcardExpansionNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> Domain -> Either String NSEC3_WildcardExpansion
wildcardExpansionNSEC3 zone n3s qname ncn = getNSEC3Result (NSEC3.get_wildcardExpansion ncn) zone n3s qname

wildcardNoDataNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> TYPE -> Either String NSEC3_WildcardNoData
wildcardNoDataNSEC3 zone ranges qname qtype = getNSEC3Result (NSEC3.get_wildcardNoData qtype) zone ranges qname

detectNSEC3 :: Domain -> [NSEC3_Range] -> Domain -> TYPE -> Either String NSEC3_Result
detectNSEC3 zone ranges qname qtype = getNSEC3Result (NSEC3.detect qtype) zone ranges qname

---

zipSigsNSEC :: [ResourceRecord] -> (String -> a) -> ([(ResourceRecord, NSEC_Range, [(RD_RRSIG, TTL)])] -> a) -> a
zipSigsNSEC = NRange.zipSigsets NSEC.rangeImpl

---

nameErrorNSEC :: Domain -> [NSEC_Range] -> Domain -> Either String NSEC_NameError
nameErrorNSEC = NSEC.getResult NSEC.get_nameError

noDataNSEC :: Domain -> [NSEC_Range] -> Domain -> TYPE -> Either String NSEC_NoData
noDataNSEC zone ranges qname qtype = NSEC.getResult (NSEC.get_noData qtype) zone ranges qname

unsignedDelegationNSEC :: Domain -> [NSEC_Range] -> Domain -> Either String NSEC_UnsignedDelegation
unsignedDelegationNSEC zone = NSEC.getResult (NSEC.get_unsignedDelegation zone) zone

wildcardExpansionNSEC :: Domain -> [NSEC_Range] -> Domain -> Either String NSEC_WildcardExpansion
wildcardExpansionNSEC = NSEC.getResult NSEC.get_wildcardExpansion

wildcardNoDataNSEC :: Domain -> [NSEC_Range] -> Domain -> TYPE -> Either String NSEC_WildcardNoData
wildcardNoDataNSEC zone ranges qname qtype = NSEC.getResult (NSEC.get_wildcardNoData qtype) zone ranges qname

detectNSEC :: Domain -> [NSEC_Range] -> Domain -> TYPE -> Either String NSEC_Result
detectNSEC zone ranges qname qtype = NSEC.getResult (NSEC.detect zone qtype) zone ranges qname

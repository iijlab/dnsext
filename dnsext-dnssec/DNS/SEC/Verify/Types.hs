{-# LANGUAGE ExistentialQuantification #-}

module DNS.SEC.Verify.Types where

-- dnsext-types

-- this package
import DNS.SEC.Imports
import DNS.SEC.Types (PubKey, RD_NSEC, RD_NSEC3)
import DNS.Types

{- FOURMOLU_DISABLE -}
data RRSIGImpl
    = forall pubkey sig.
      RRSIGImpl
    { rrsigIDecodePubKey :: PubKey -> Either String pubkey
    , rrsigIDecodeSignature :: Opaque -> Either String sig
    , rrsigIVerify :: pubkey -> sig -> ByteString -> Either String Bool
    }

data DSImpl
    = forall digest.
      DSImpl
    { dsIGetDigest :: ByteString -> digest
    , dsIVerify    :: digest -> ByteString -> Bool
    }

data NSEC3Impl
    = forall hash.
      NSEC3Impl
    { nsec3IGetHash  :: ByteString -> hash
    , nsec3IGetBytes :: hash -> ByteString
    }
{- FOURMOLU_ENABLE -}

---

-- | owner name and NSEC3 rdata express hashed domain-name range
type NSEC3_Range = (Domain, RD_NSEC3)

-- | range and qname which is owner of it or covered by it
type NSEC3_Witness = (NSEC3_Range, Domain)

{- FOURMOLU_DISABLE -}
data NSEC3_NameError
    = NSEC3_NameError
    { nsec3_nameError_closest_match     :: NSEC3_Witness
    , nsec3_nameError_next_closer_cover :: NSEC3_Witness
    , nsec3_nameError_wildcard_cover    :: NSEC3_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc5155#appendix-B.1 -}
    deriving (Show)

newtype NSEC3_NoData
    = NSEC3_NoData
    { nsec3_noData_closest_match :: NSEC3_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc5155#appendix-B.2
         https://datatracker.ietf.org/doc/html/rfc5155#appendix-B.6 -}
    deriving (Show)

data NSEC3_UnsignedDelegation
    = NSEC3_UnsignedDelegation
    { nsec3_unsignedDelegation_closest_match     :: NSEC3_Witness
    , nsec3_unsignedDelegation_next_closer_cover :: NSEC3_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc5155#appendix-B.3 -}
    deriving (Show)

newtype NSEC3_WildcardExpansion
    = NSEC3_WildcardExpansion
    { nsec3_wildcardExpansion_next_closer_cover :: NSEC3_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc5155#appendix-B.4 -}
    deriving (Show)

data NSEC3_WildcardNoData
    = NSEC3_WildcardNoData
    { nsec3_wildcardNodata_closest_match     :: NSEC3_Witness
    , nsec3_wildcardNodata_next_closer_cover :: NSEC3_Witness
    , nsec3_wildcardNodata_wildcard_match    :: NSEC3_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc5155#appendix-B.5 -}
    deriving (Show)

{- FOURMOLU_ENABLE -}

data NSEC3_Result
    = N3R_NameError NSEC3_NameError
    | N3R_NoData NSEC3_NoData
    | N3R_UnsignedDelegation NSEC3_UnsignedDelegation
    | N3R_WildcardExpansion NSEC3_WildcardExpansion
    | N3R_WildcardNoData NSEC3_WildcardNoData
    deriving (Show)

---

type NSEC_Range = (Domain, RD_NSEC)

type NSEC_Witness = (NSEC_Range, Domain)

{- FOURMOLU_DISABLE -}
data NSEC_NameError
    = NSEC_NameError
    { nsec_nameError_qname_cover    :: NSEC_Witness
    , nsec_nameError_wildcard_cover :: NSEC_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc4035#appendix-B.2 -}
    deriving (Show)

newtype NSEC_NoData
    = NSEC_NoData
    { nsec_noData_qname_match :: NSEC_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc4035#appendix-B.3 -}
    deriving (Show)

newtype NSEC_UnsignedDelegation
    = NSEC_UnsignedDelegation
    { nsec_unsignedDelegation_ns_qname_cover :: NSEC_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc4035#appendix-B.5 -}
    deriving (Show)

newtype NSEC_WildcardExpansion
    = NSEC_WildcardExpansion
    { nsec_wildcardExpansion_qname_cover :: NSEC_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc4035#appendix-B.6 -}
    deriving (Show)

data NSEC_WildcardNoData
    = NSEC_WildcardNoData
    { nsec_wildcardNoData_qname_cover    :: NSEC_Witness
    , nsec_wildcardNoData_wildcard_match :: NSEC_Witness
    } {- https://datatracker.ietf.org/doc/html/rfc4035#appendix-B.7 -}
    deriving (Show)
{- FOURMOLU_ENABLE -}

data NSEC_Result
    = NSECR_NameError NSEC_NameError
    | NSECR_NoData NSEC_NoData
    | NSECR_UnsignedDelegation NSEC_UnsignedDelegation
    | NSECR_WildcardExpansion NSEC_WildcardExpansion
    | NSECR_WildcardNoData NSEC_WildcardNoData
    deriving (Show)

---

data NSECxWitnessType
    = NwNameError
    | NwNoData
    | NwUnsignedDelegation
    | NwWildcardExpantion
    | NwWildcardNoData
    deriving (Eq, Show)

class NSECxWitness w where
    witnessName :: w -> String
    witnessType :: w -> NSECxWitnessType

---

instance NSECxWitness NSEC3_NameError where
    witnessName _ = "NSEC3 NameError"
    witnessType _ = NwNameError

instance NSECxWitness NSEC3_NoData where
    witnessName _ = "NSEC3 NoData"
    witnessType _ = NwNoData

instance NSECxWitness NSEC3_UnsignedDelegation where
    witnessName _ = "NSEC3 UnsignedDelegation"
    witnessType _ = NwUnsignedDelegation

instance NSECxWitness NSEC3_WildcardExpansion where
    witnessName _ = "NSEC3 WildcardExpansion"
    witnessType _ = NwWildcardExpantion

instance NSECxWitness NSEC3_WildcardNoData where
    witnessName _ = "NSEC3 WildcardNoData"
    witnessType _ = NwWildcardNoData

---

instance NSECxWitness NSEC_NameError where
    witnessName _ = "NSEC NameError"
    witnessType _ = NwNameError

instance NSECxWitness NSEC_NoData where
    witnessName _ = "NSEC NoData"
    witnessType _ = NwNoData

instance NSECxWitness NSEC_UnsignedDelegation where
    witnessName _ = "NSEC UnsignedDelegation"
    witnessType _ = NwUnsignedDelegation

instance NSECxWitness NSEC_WildcardExpansion where
    witnessName _ = "NSEC WildcardExpansion"
    witnessType _ = NwWildcardExpantion

instance NSECxWitness NSEC_WildcardNoData where
    witnessName _ = "NSEC WildcardNoData"
    witnessType _ = NwWildcardNoData

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DNS.Types.RData where

import Data.Char (chr)
import Data.IP (IPv4, IPv6, fromIPv4, fromIPv6b, toIPv4, toIPv6b)
import Data.Word8

import DNS.Types.Domain
import DNS.Types.EDNS
import DNS.Types.Imports
import DNS.Types.Opaque.Internal (
    Opaque,
    getLenOpaque,
    getOpaque,
    putLenOpaque,
    putOpaque,
 )
import qualified DNS.Types.Opaque.Internal as Opaque
import DNS.Types.Seconds
import DNS.Types.Serial
import DNS.Types.Type
import DNS.Wire

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short

---------------------------------------------------------------

class (Typeable a, Eq a, Show a) => ResourceData a where
    resourceDataType :: a -> TYPE
    resourceDataSize :: a -> Int
    putResourceData :: CanonicalFlag -> a -> Builder ()

---------------------------------------------------------------

-- | A type to uniform 'ResourceData' 'a'.
data RData = forall a. (Typeable a, Eq a, Show a, ResourceData a) => RData a

-- | Extracting the original type.
fromRData :: Typeable a => RData -> Maybe a
fromRData (RData x) = cast x

-- | Wrapping the original type with 'RData'.
toRData :: (Typeable a, ResourceData a) => a -> RData
toRData = RData

instance Show RData where
    show (RData x) = show x

instance Eq RData where
    x@(RData xi) == y@(RData yi) = typeOf x == typeOf y && Just xi == cast yi

-- | Getting 'TYPE' of 'RData'.
rdataType :: RData -> TYPE
rdataType (RData x) = resourceDataType x

rdataSize :: RData -> Int
rdataSize (RData x) = resourceDataSize x

putRData :: CanonicalFlag -> RData -> Builder ()
putRData cf (RData x) = putResourceData cf x

rdataField :: forall a b. Typeable a => RData -> (a -> b) -> Maybe b
rdataField rd f = case fromRData rd of
    Nothing -> Nothing
    Just (x :: a) -> Just $ f x

---------------------------------------------------------------

prettyShowRData :: RData -> String
prettyShowRData rd = loop (0 :: Int) $ show rd
  where
    loop _ [] = []
    loop n (c : cs)
        | c == '=' && take 5 cs `elem` dows = c : take 5 cs ++ loop n (drop 5 cs)
        | c == '[' = c : bracket (1 :: Int) n cs
        | c == '"' = c : quote n cs
        | c == '{' = "{\n" ++ concat (replicate (n + 1) "    ") ++ loop (n + 1) cs
        | c == '}' = "}" ++ loop (n - 1) cs
        | c == ',' = "\n" ++ concat (replicate (n - 1) "    ") ++ "  ," ++ loop n cs
        | otherwise = c : loop n cs
    -- it is assumed that "struct"s are not contained in a list
    bracket _ _ [] = []
    bracket 0 n ccs = loop n ccs
    bracket m n (c : cs)
        | c == '[' = c : bracket (m + 1) n cs
        | c == ']' = c : bracket (m - 1) n cs
        | otherwise = c : bracket m n cs
    quote _ [] = []
    quote n (c : cs)
        | c == '"' = c : loop n cs
        | otherwise = c : quote n cs
    dows = [" Sun,", " Mon,", " Tue,", " Wed,", " Thu,", " Fri,", " Sat,"]

---------------------------------------------------------------

-- | IPv4 Address (RFC1035)
newtype RD_A = RD_A
    { a_ipv4 :: IPv4
    -- ^ Setter/getter for 'IPv4'.
    }
    deriving (Eq, Ord)

instance ResourceData RD_A where
    resourceDataType _ = A
    resourceDataSize _ = 4
    putResourceData _ (RD_A ipv4) = \wbuf _ -> mapM_ (putInt8 wbuf) $ fromIPv4 ipv4

get_a :: Int -> Parser RD_A
get_a _ rbuf _ = RD_A . toIPv4 <$> getNBytes rbuf 4

instance Show RD_A where
    show (RD_A ipv4) = show ipv4

-- | Smart constructor.
rd_a :: IPv4 -> RData
rd_a ipv4 = toRData $ RD_A ipv4

----------------------------------------------------------------

-- | An authoritative name serve (RFC1035)
newtype RD_NS = RD_NS
    { ns_domain :: Domain
    -- ^ Setter/getter for 'Domain'.
    }
    deriving (Eq, Ord)

instance ResourceData RD_NS where
    resourceDataType _ = NS
    resourceDataSize (RD_NS d) = domainSize d
    putResourceData cf (RD_NS d) = putDomainRFC1035 cf d

get_ns :: Int -> Parser RD_NS
get_ns _ rbuf ref = RD_NS <$> getDomainRFC1035 rbuf ref

instance Show RD_NS where
    show (RD_NS d) = toRepresentation d

-- | Smart constructor.
rd_ns :: Domain -> RData
rd_ns d = toRData $ RD_NS d

----------------------------------------------------------------

-- | The canonical name for an alias (RFC1035)
newtype RD_CNAME = RD_CNAME
    { cname_domain :: Domain
    -- ^ Setter/getter for 'Domain'.
    }
    deriving (Eq, Ord)

instance ResourceData RD_CNAME where
    resourceDataType _ = CNAME
    resourceDataSize (RD_CNAME d) = domainSize d
    putResourceData cf (RD_CNAME d) = putDomainRFC1035 cf d

get_cname :: Int -> Parser RD_CNAME
get_cname _ rbuf ref = RD_CNAME <$> getDomainRFC1035 rbuf ref

instance Show RD_CNAME where
    show (RD_CNAME d) = toRepresentation d

-- | Smart constructor.
rd_cname :: Domain -> RData
rd_cname d = toRData $ RD_CNAME d

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Marks the start of a zone of authority (RFC1035)
data RD_SOA = RD_SOA
    { soa_mname   :: Domain
    -- ^ Setter/getter for mname
    , soa_rname   :: Mailbox
    -- ^ Setter/getter for rname
    , soa_serial  :: Serial
    -- ^ Setter/getter for serial
    , soa_refresh :: Seconds
    -- ^ Setter/getter for refresh
    , soa_retry   :: Seconds
    -- ^ Setter/getter for retry
    , soa_expire  :: Seconds
    -- ^ Setter/getter for expire
    , soa_minimum :: Seconds
    -- ^ Setter/getter for minimum
    }
    deriving (Eq, Ord, Show)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance ResourceData RD_SOA where
    resourceDataType _ = SOA
    resourceDataSize RD_SOA{..} = domainSize soa_mname + mailboxSize soa_rname + 20
    putResourceData cf RD_SOA{..} = \wbuf ref -> do
        putDomainRFC1035  cf soa_mname   wbuf ref
        putMailboxRFC1035 cf soa_rname   wbuf ref
        put32 wbuf           $ unSerial soa_serial
        putSeconds           soa_refresh wbuf ref
        putSeconds           soa_retry   wbuf ref
        putSeconds           soa_expire  wbuf ref
        putSeconds           soa_minimum wbuf ref
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
get_soa :: Int -> Parser RD_SOA
get_soa _ rbuf ref = do
    soa_mname   <- getDomainRFC1035 rbuf ref
    soa_rname   <- getMailboxRFC1035 rbuf ref
    soa_serial  <- Serial <$> get32 rbuf
    soa_refresh <- getSeconds rbuf ref
    soa_retry   <- getSeconds rbuf ref
    soa_expire  <- getSeconds rbuf ref
    soa_minimum <- getSeconds rbuf ref
    return RD_SOA{..}
{- FOURMOLU_ENABLE -}

-- | Smart constructor.
rd_soa
    :: Domain -> Mailbox -> Word32 -> Seconds -> Seconds -> Seconds -> Seconds -> RData
rd_soa a b c d e f g = toRData $ RD_SOA a b (Serial c) d e f g

----------------------------------------------------------------

-- | NULL RR (EXPERIMENTAL, RFC1035).
newtype RD_NULL = RD_NULL
    { null_opaque :: Opaque
    -- ^ Setter/getter for 'Opaque'.
    }
    deriving (Eq, Ord)

instance ResourceData RD_NULL where
    resourceDataType _ = NULL
    resourceDataSize (RD_NULL o) = Opaque.length o
    putResourceData _ (RD_NULL o) = putOpaque o

get_null :: Int -> Parser RD_NULL
get_null len rbuf ref = RD_NULL <$> getOpaque len rbuf ref

instance Show RD_NULL where
    show (RD_NULL o) = show o

rd_null :: Opaque -> RData
rd_null = toRData . RD_NULL

----------------------------------------------------------------

-- Host information (RFC1035)
data RD_HINFO = RD_HINFO
    { hinfo_cpu :: ShortByteString
    , hinfo_os :: ShortByteString
    }
    deriving (Eq, Ord)

instance ResourceData RD_HINFO where
    resourceDataType _ = HINFO
    resourceDataSize (RD_HINFO cpu os) = Short.length cpu + Short.length os + 2
    putResourceData _ (RD_HINFO cpu os) = \wbuf _ -> do
        putLenShortByteString wbuf cpu
        putLenShortByteString wbuf os

get_hinfo :: Int -> Parser RD_HINFO
get_hinfo _ rbuf _ = RD_HINFO <$> getCharString <*> getCharString
  where
    getCharString = getInt8 rbuf >>= getNShortByteString rbuf

instance Show RD_HINFO where
    show (RD_HINFO cpu os) = toString cpu ++ " " ++ toString os
      where
        toString = C8.unpack . Short.fromShort

-- | Smart constructor.
rd_hinfo :: ShortByteString -> ShortByteString -> RData
rd_hinfo cpu os =
    toRData $
        RD_HINFO
            { hinfo_cpu = cpu
            , hinfo_os = os
            }

----------------------------------------------------------------

-- | A domain name pointer (RFC1035)
newtype RD_PTR = RD_PTR
    { ptr_domain :: Domain
    -- ^ Setter/getter for 'Domain'
    }
    deriving (Eq, Ord)

instance ResourceData RD_PTR where
    resourceDataType _ = PTR
    resourceDataSize (RD_PTR d) = domainSize d
    putResourceData cf (RD_PTR d) = putDomainRFC1035 cf d

get_ptr :: Int -> Parser RD_PTR
get_ptr _ rbuf ref = RD_PTR <$> getDomainRFC1035 rbuf ref

instance Show RD_PTR where
    show (RD_PTR d) = toRepresentation d

-- | Smart constructor.
rd_ptr :: Domain -> RData
rd_ptr d = toRData $ RD_PTR d

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Mail exchange (RFC1035)
data RD_MX = RD_MX
    { mx_preference :: Word16
    -- ^ Setter/getter for preference
    , mx_exchange   :: Domain
    -- ^ Setter/getter for 'Domain'
    }
    deriving (Eq, Ord)
{- FOURMOLU_ENABLE -}

instance Show RD_MX where
    show RD_MX{..} = show mx_preference ++ " " ++ toRepresentation mx_exchange

{- FOURMOLU_DISABLE -}
instance ResourceData RD_MX where
    resourceDataType _ = MX
    resourceDataSize RD_MX{..} = 2 + domainSize mx_exchange
    putResourceData cf RD_MX{..} = \wbuf ref -> do
        put16          wbuf mx_preference
        putDomainRFC1035 cf mx_exchange wbuf ref
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
get_mx :: Int -> Parser RD_MX
get_mx _ rbuf ref = do
    mx_preference <- get16            rbuf
    mx_exchange   <- getDomainRFC1035 rbuf ref
    return RD_MX {..}
{- FOURMOLU_ENABLE -}

-- | Smart constructor.
rd_mx :: Word16 -> Domain -> RData
rd_mx a b = toRData $ RD_MX a b

----------------------------------------------------------------

-- | Text strings (RFC1035)
newtype RD_TXT = RD_TXT
    { txt_opaque :: [Opaque]
    -- ^ Setter/getter for 'Opaque'
    }
    deriving (Eq, Ord)

{- FOURMOLU_DISABLE -}
instance ResourceData RD_TXT where
    resourceDataType _ = TXT

    {- https://datatracker.ietf.org/doc/html/rfc1035#section-3.3.14  TXT RDATA format
         "TXT-DATA    One or more <character-string>s."
       https://datatracker.ietf.org/doc/html/rfc1035#section-3.3  Standard RRs
         "<character-string> is a single length octet followed by that number of characters."
       https://datatracker.ietf.org/doc/html/rfc6763#section-6.1
         "* An empty (zero-length) TXT record.
            (This is not strictly legal, but should one be received,
            it should be interpreted as the same as a single empty string.)" -}
    resourceDataSize (RD_TXT []) = 1
    resourceDataSize (RD_TXT os) =
        let len o = 1 + Opaque.length o
         in sum $ map len os
    putResourceData _ (RD_TXT os) = putTXT os
      where
        {- RFC 6763 - not strictly legal, should be interpreted as the same as a single empty string -}
        putTXT [] wbuf ref = putLenOpaque "" wbuf ref
        putTXT xs wbuf ref = mapM_ (\o -> putLenOpaque o wbuf ref) xs
{- FOURMOLU_ENABLE -}

get_txt :: Int -> Parser RD_TXT
get_txt len rbuf ref = RD_TXT <$> sGetMany "TXT RR string" len getLenOpaque rbuf ref

instance Show RD_TXT where
    show (RD_TXT os) = unwords ['"' : conv o '"' | o <- os]
      where
        conv t c = Opaque.foldr escape [c] t
        escape :: Word8 -> [Char] -> [Char]
        escape w s
            | w == _quotedbl = '\\' : c : s
            | w == _backslash = '\\' : c : s
            | _space <= w && w <= _tilde = c : s
            | otherwise = '\\' : ddd w s
          where
            c = w8ToChar w
        ddd w s =
            let (q100, r100) = divMod w 100
                (q10, r10) = divMod r100 10
             in w8ToDigit q100 : w8ToDigit q10 : w8ToDigit r10 : s
        w8ToDigit w = chr $ fromIntegral (w + _0)
        w8ToChar = chr . fromIntegral

-- | Smart constructor for multiple txt string.
rd_txt_n :: [Opaque] -> RData
rd_txt_n os = toRData $ RD_TXT os

-- | Smart constructor.
rd_txt :: Opaque -> RData
rd_txt o = rd_txt_n [o]

----------------------------------------------------------------

-- | Responsible Person (RFC1183)
data RD_RP = RD_RP
    { rp_mbox :: Mailbox
    , rp_domain :: Domain
    }
    deriving (Eq, Ord, Show)

instance ResourceData RD_RP where
    resourceDataType _ = RP
    resourceDataSize (RD_RP mbox d) = mailboxSize mbox + domainSize d
    putResourceData cf (RD_RP mbox d) = do
        _ <- putMailbox cf mbox
        putDomain cf d

get_rp :: Int -> Parser RD_RP
get_rp _ rbuf ref = RD_RP <$> getMailbox rbuf ref <*> getDomain rbuf ref

-- | Smart constructor.
rd_rp :: Mailbox -> Domain -> RData
rd_rp a b = toRData $ RD_RP a b

----------------------------------------------------------------

-- | IPv6 Address (RFC3596)
newtype RD_AAAA = RD_AAAA
    { aaaa_ipv6 :: IPv6
    -- ^ Setter/getter for 'IPv6'
    }
    deriving (Eq, Ord)

instance ResourceData RD_AAAA where
    resourceDataType _ = AAAA
    resourceDataSize _ = 16
    putResourceData _ (RD_AAAA ipv6) = \wbuf _ -> mapM_ (putInt8 wbuf) $ fromIPv6b ipv6

get_aaaa :: Int -> Parser RD_AAAA
get_aaaa _ rbuf _ = RD_AAAA . toIPv6b <$> getNBytes rbuf 16

instance Show RD_AAAA where
    show (RD_AAAA ipv6) = show ipv6

-- | Smart constructor.
rd_aaaa :: IPv6 -> RData
rd_aaaa ipv6 = toRData $ RD_AAAA ipv6

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Server Selection (RFC2782)
data RD_SRV = RD_SRV
    { srv_priority :: Word16
    , srv_weight   :: Word16
    , srv_port     :: Word16
    , srv_target   :: Domain
    }
    deriving (Eq, Ord, Show)
{- FOURMOLU_ENABLE -}

instance ResourceData RD_SRV where
    resourceDataType _ = SRV
    resourceDataSize RD_SRV{..} = 6 + domainSize srv_target
    putResourceData cf RD_SRV{..} = \wbuf ref -> do
        put16 wbuf srv_priority
        put16 wbuf srv_weight
        put16 wbuf srv_port
        putDomain cf srv_target wbuf ref

{- FOURMOLU_DISABLE -}
get_srv :: Int -> Parser RD_SRV
get_srv _ rbuf ref = do
    srv_priority <- get16 rbuf
    srv_weight   <- get16 rbuf
    srv_port     <- get16 rbuf
    srv_target   <- getDomain rbuf ref
    return RD_SRV {..}
{- FOURMOLU_ENABLE -}

-- | Smart constructor.
rd_srv :: Word16 -> Word16 -> Word16 -> Domain -> RData
rd_srv a b c d = toRData $ RD_SRV a b c d

----------------------------------------------------------------

-- | DNAME (RFC6672)
newtype RD_DNAME = RD_DNAME
    { dname_target :: Domain
    }
    deriving (Eq, Ord)

instance ResourceData RD_DNAME where
    resourceDataType _ = DNAME
    resourceDataSize (RD_DNAME d) = domainSize d
    putResourceData cf (RD_DNAME d) = putDomain cf d

get_dname :: Int -> Parser RD_DNAME
get_dname _ rbuf ref = RD_DNAME <$> getDomain rbuf ref

instance Show RD_DNAME where
    show (RD_DNAME d) = toRepresentation d

-- | Smart constructor.
rd_dname :: Domain -> RData
rd_dname d = toRData $ RD_DNAME d

----------------------------------------------------------------

-- | OPT (RFC6891)
newtype RD_OPT = RD_OPT
    { opt_odata :: [OData]
    }
    deriving (Eq)

instance ResourceData RD_OPT where
    resourceDataType _ = OPT
    resourceDataSize _ = 0 -- fixme
    putResourceData _ (RD_OPT options) = \wbuf ref -> mapM_ (\o -> putOData o wbuf ref) options

instance Show RD_OPT where
    show (RD_OPT options) = show options

-- | Smart constructor.
rd_opt :: [OData] -> RData
rd_opt x = toRData $ RD_OPT x

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | TLSA (RFC6698)
data RD_TLSA = RD_TLSA
    { tlsa_usage         :: Word8
    , tlsa_selector      :: Word8
    , tlsa_matching_type :: Word8
    , tlsa_assoc_data    :: Opaque
    }
    deriving (Eq, Ord, Show)
{- FOURMOLU_ENABLE -}

instance ResourceData RD_TLSA where
    resourceDataType _ = TLSA
    resourceDataSize RD_TLSA{..} = 3 + Opaque.length tlsa_assoc_data
    putResourceData _ RD_TLSA{..} = \wbuf ref -> do
        put8 wbuf tlsa_usage
        put8 wbuf tlsa_selector
        put8 wbuf tlsa_matching_type
        putOpaque tlsa_assoc_data wbuf ref

{- FOURMOLU_DISABLE -}
get_tlsa :: Int -> Parser RD_TLSA
get_tlsa len rbuf ref = do
    tlsa_usage         <- get8 rbuf
    tlsa_selector      <- get8 rbuf
    tlsa_matching_type <- get8 rbuf
    tlsa_assoc_data    <- getOpaque (len - 3) rbuf ref
    return RD_TLSA {..}
{- FOURMOLU_ENABLE -}

-- | Smart constructor.
rd_tlsa :: Word8 -> Word8 -> Word8 -> Opaque -> RData
rd_tlsa a b c d = toRData $ RD_TLSA a b c d

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Transaction signature (RFC 8945 Sec 4.2).
--
--   A meta RR: it authenticates one message with a secret shared with
--   one peer, and is never stored in a zone.  The owner name is the name
--   of the key, the class is ANY and the TTL is 0.
--
--   The MAC Size and Other Len fields of the wire format are the lengths
--   of 'tsig_mac' and 'tsig_other', so they are not kept separately.
data RD_TSIG = RD_TSIG
    { tsig_algorithm   :: Domain
    -- ^ Name of the algorithm, such as @hmac-sha256@
    , tsig_time_signed :: Word64
    -- ^ Seconds since the epoch, 48 bits on the wire
    , tsig_fudge       :: Word16
    -- ^ Seconds of difference from 'tsig_time_signed' permitted
    , tsig_mac         :: Opaque
    -- ^ The message authentication code itself
    , tsig_original_id :: Word16
    -- ^ Identifier of the message this signs
    , tsig_error       :: Word16
    -- ^ Extended RCODE covering TSIG processing
    , tsig_other       :: Opaque
    -- ^ Meaningful only for some errors
    }
    deriving (Eq, Ord, Show)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance ResourceData RD_TSIG where
    resourceDataType _ = TSIG
    -- algorithm + 48 bit time + 5 * 16 bit + the two variable fields
    resourceDataSize RD_TSIG{..} =
        domainSize tsig_algorithm
            + 16
            + Opaque.length tsig_mac
            + Opaque.length tsig_other
    putResourceData cf RD_TSIG{..} = \wbuf ref -> do
        -- RFC 8945 Sec 4.2: the algorithm name MUST NOT be compressed.
        putDomain cf tsig_algorithm wbuf ref
        put48 wbuf           tsig_time_signed
        put16 wbuf           tsig_fudge
        put16 wbuf           $ fromIntegral $ Opaque.length tsig_mac
        putOpaque            tsig_mac wbuf ref
        put16 wbuf           tsig_original_id
        put16 wbuf           tsig_error
        put16 wbuf           $ fromIntegral $ Opaque.length tsig_other
        putOpaque            tsig_other wbuf ref
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
get_tsig :: Int -> Parser RD_TSIG
get_tsig _ rbuf ref = do
    -- getDomain rejects a compressed name, which is what RFC 8945 asks.
    tsig_algorithm   <- getDomain rbuf ref
    tsig_time_signed <- get48 rbuf
    tsig_fudge       <- get16 rbuf
    maclen           <- getInt16 rbuf
    tsig_mac         <- getOpaque maclen rbuf ref
    tsig_original_id <- get16 rbuf
    tsig_error       <- get16 rbuf
    otherlen         <- getInt16 rbuf
    tsig_other       <- getOpaque otherlen rbuf ref
    return RD_TSIG{..}
{- FOURMOLU_ENABLE -}

-- | Smart constructor.
rd_tsig :: Domain -> Word64 -> Word16 -> Opaque -> Word16 -> Word16 -> Opaque -> RData
rd_tsig a b c d e f g = toRData $ RD_TSIG a b c d e f g

----------------------------------------------------------------

-- | Unknown resource data
data RD_Unknown = RD_Unknown TYPE Opaque deriving (Eq, Ord)

instance Show RD_Unknown where
    show (RD_Unknown typ o) = "RD_Unknown(" ++ show (fromTYPE typ) ++ ") " ++ show o

instance ResourceData RD_Unknown where
    resourceDataType (RD_Unknown typ _) = typ
    resourceDataSize (RD_Unknown _ o) = Opaque.length o
    putResourceData _ (RD_Unknown _ o) = putOpaque o

-- | Smart constructor.
rd_unknown :: TYPE -> Opaque -> RData
rd_unknown a b = toRData $ RD_Unknown a b

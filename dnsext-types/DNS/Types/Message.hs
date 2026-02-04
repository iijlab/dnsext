{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Types.Message where

import qualified Control.Exception as E
import Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as ST

import DNS.Types.Dict
import DNS.Types.Domain
import DNS.Types.Error (DNSError (DecodeError))
import DNS.Types.EDNS
import DNS.Types.Imports
import DNS.Types.RData
import DNS.Types.Seconds
import DNS.Types.Type
import DNS.Wire

----------------------------------------------------------------

-- | Data type representing the optional EDNS pseudo-header of a 'DNSMessage'
-- When a single well-formed @OPT@ 'ResourceRecord' was present in the
-- message's additional section, it is decoded to an 'EDNS' record and and
-- stored in the message 'ednsHeader' field.  The corresponding @OPT RR@ is
-- then removed from the additional section.
--
-- When the constructor is 'NoEDNS', no @EDNS OPT@ record was present in the
-- message additional section.  When 'InvalidEDNS', the message holds either a
-- malformed OPT record or more than one OPT record, which can still be found
-- in (have not been removed from) the message additional section.
--
-- The EDNS OPT record augments the message error status with an 8-bit field
-- that forms 12-bit extended RCODE when combined with the 4-bit RCODE from the
-- unextended DNS header.  In EDNS messages it is essential to not use just the
-- bare 4-bit 'RCODE' from the original DNS header.  Therefore, in order to
-- avoid potential misinterpretation of the response 'RCODE', when the OPT
-- record is decoded, the upper eight bits of the error status are
-- automatically combined with the 'rcode' of the message header, so that there
-- is only one place in which to find the full 12-bit result.  Therefore, the
-- decoded 'EDNS' pseudo-header, does not hold any error status bits.
--
-- The reverse process occurs when encoding messages.  The low four bits of the
-- message header 'rcode' are encoded into the wire-form DNS header, while the
-- upper eight bits are encoded as part of the OPT record.  In DNS responses with
-- an 'rcode' larger than 15, EDNS extensions SHOULD be enabled by providing a
-- value for 'ednsHeader' with a constructor of 'EDNSheader'.  If EDNS is not
-- enabled in such a message, in order to avoid truncation of 'RCODE' values
-- that don't fit in the non-extended DNS header, the encoded wire-form 'RCODE'
-- is set to 'FormatErr'.
--
-- When encoding messages for transmission, the 'ednsHeader' is used to
-- generate the additional OPT record.  Do not add explicit @OPT@ records
-- to the aditional section, configure EDNS via the 'EDNSheader' instead.
data EDNSheader
    = -- | A valid EDNS message
      EDNSheader EDNS
    | -- | A valid non-EDNS message
      NoEDNS
    | -- | Multiple or bad additional @OPT@ RRs
      InvalidEDNS
    deriving (Eq, Show)

-- | Return the second argument for EDNS messages, otherwise the third.
ifEDNS
    :: EDNSheader
    -- ^ EDNS pseudo-header
    -> a
    -- ^ Value to return for EDNS messages
    -> a
    -- ^ Value to return for non-EDNS messages
    -> a
ifEDNS (EDNSheader _) a _ = a
ifEDNS _ _ b = b
{-# INLINE ifEDNS #-}

-- | Return the output of a function applied to the EDNS pseudo-header if EDNS
--   is enabled, otherwise return a default value.
mapEDNS
    :: EDNSheader
    -- ^ EDNS pseudo-header
    -> (EDNS -> a)
    -- ^ Function to apply to 'EDNS' value
    -> a
    -- ^ Default result for non-EDNS messages
    -> a
mapEDNS (EDNSheader eh) f _ = f eh
mapEDNS _ _ a = a
{-# INLINE mapEDNS #-}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | DNS message format for queries and replies.
data DNSMessage = DNSMessage
    { identifier :: Identifier
    -- ^ Query or response identifier.
    , opcode     :: OPCODE
    -- ^ Kind of query.
    , rcode      :: RCODE
    -- ^ The full 12-bit extended RCODE when EDNS is in use.
    -- Should always be zero in well-formed requests.
    -- When decoding replies, the high eight bits from
    -- any EDNS response are combined with the 4-bit
    -- RCODE from the DNS header.  When encoding
    -- replies, if no EDNS OPT record is provided, RCODE
    -- values > 15 are mapped to 'FormatErr'.
    , flags      :: DNSFlags
    -- ^ Flags
    , ednsHeader :: EDNSheader
    -- ^ EDNS pseudo-header
    , question   :: Question
    -- ^ The question for the name server
    , answer     :: Answers
    -- ^ RRs answering the question
    , authority  :: AuthorityRecords
    -- ^ RRs pointing toward an authority
    , additional :: AdditionalRecords
    -- ^ RRs holding additional information
    }
    deriving (Eq, Show)
{- FOURMOLU_ENABLE -}

-- | An identifier assigned by the program that
--   generates any kind of query.
type Identifier = Word16

arCountEDNS :: DNSMessage -> Int
arCountEDNS DNSMessage{..} = ifEDNS ednsHeader (arCount0 + 1) arCount0
  where
    arCount0 = length additional

putDNSMessage :: DNSMessage -> Builder ()
putDNSMessage DNSMessage{..} wbuf ref = do
    putIdentifier wbuf identifier
    putDNSFlags (flags, opcode, rcode') wbuf ref
    putNums
    putQ question
    mapM_ putRR answer
    mapM_ putRR authority
    mapM_ putRR ad
  where
    putIdentifier = put16
    putNums =
        mapM_
            (putInt16 wbuf)
            [ 1
            , length answer
            , length authority
            , length ad
            ]
    putQ q = putQuestion Original q wbuf ref
    putRR rr = putResourceRecord Original rr wbuf ref
    rcode' = (ifEDNS ednsHeader <$> id <*> nonEDNSrcode) rcode
      where
        nonEDNSrcode code
            | fromRCODE code < 16 = code
            | otherwise = FormatErr -- `BadRCODE` case is also handled
    ad = prependOpt additional
      where
        {- An EDNS header encoded into an OPT RR extends the additional section by exactly one.
           The following implementation clarifies the rule. -}
        prependOpt ads = mapEDNS ednsHeader (\edns -> fromEDNS (fromRCODE rcode') edns : ads) ads
        fromEDNS :: Word16 -> EDNS -> ResourceRecord
        fromEDNS rc' edns = ResourceRecord name' type' class' ttl' rdata'
          where
            name' = "."
            type' = OPT
            class' = CLASS (maxUdpSize `min` (minUdpSize `max` ednsUdpSize edns))
            ttl0' = fromIntegral (rc' .&. 0xff0) `shiftL` 20
            vers' = fromIntegral (ednsVersion edns) `shiftL` 16
            ttl'
                | ednsDnssecOk edns = ttl0' `setBit` 15 .|. vers'
                | otherwise = ttl0' .|. vers'
            rdata' = RData $ RD_OPT $ ednsOptions edns

{- FOURMOLU_DISABLE -}
getDNSMessage :: Parser DNSMessage
getDNSMessage rbuf ref = do
    identifier <- getIdentifier rbuf
    (flags, opcode, rc0) <- getDNSFlags rbuf ref
    qdCount <- getInt16 rbuf
    unless (qdCount == 1) $ E.throw $ DecodeError $ "QDCOUNT is " ++ show qdCount ++ ". must be 1"
    anCount <- getInt16 rbuf
    nsCount <- getInt16 rbuf
    arCount <- getInt16 rbuf
    question   <- getQuestion rbuf ref
    answer     <- getResourceRecords anCount rbuf ref
    authority  <- getResourceRecords nsCount rbuf ref
    addnrrs    <- getResourceRecords arCount rbuf ref
    let (opts, rest) = partition ((==) OPT . rrtype) addnrrs
        (ednsHeader, rcode) = getEDNS (fromRCODE rc0) opts
        additional = ifEDNS ednsHeader rest addnrrs
    return DNSMessage {..}
  where
    getIdentifier = get16
    -- \| Get EDNS pseudo-header and the high eight bits of the extended RCODE.
    getEDNS :: Word16 -> AdditionalRecords -> (EDNSheader, RCODE)
    getEDNS rc rrs = case rrs of
        -- The OPT Pseudo-RR / Basic Elements - https://datatracker.ietf.org/doc/html/rfc6891#section-6.1.1
        -- "When an OPT RR is included within any DNS message, it MUST be the only OPT RR in that message."
        [ResourceRecord "." OPT (CLASS udpsiz) ttl' rd]
            | Just (RD_OPT opts) <- fromRData rd -> mkEDNS udpsiz ttl' opts
        [] -> (NoEDNS, toRCODE rc)
        _ -> (InvalidEDNS, BadRCODE)
      where
        -- \| Extract EDNS information from an OPT RR.
        mkEDNS udpsiz ttl' opts =
            (EDNSheader $ EDNS vers udpsiz secok opts, toRCODE $ fromIntegral erc)
          where
            hrc = fromIntegral rc .&. 0x0f
            erc = shiftR (ttl' .&. 0xff000000) 20 .|. hrc
            secok = ttl' `testBit` 15
            vers = fromIntegral $ shiftR (ttl' .&. 0x00ff0000) 16
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | Maximum UDP size that can be advertised.  If the 'ednsUdpSize' of 'EDNS'
--   is larger, then this value is sent instead.  This value is likely to work
--   only for local nameservers on the loopback network.  Servers may enforce
--   a smaller limit.
--
-- >>> maxUdpSize
-- 16384
maxUdpSize :: Word16
maxUdpSize = 16384

-- | Minimum UDP size to advertise. If 'ednsUdpSize' of 'EDNS' is smaller,
--   then this value is sent instead.
--
-- >>> minUdpSize
-- 512
minUdpSize :: Word16
minUdpSize = 512

----------------------------------------------------------------

-- | A 'DNSMessage' template for queries with default settings for
-- the message 'DNSHeader' and 'EDNSheader'.
--
-- >>> defaultQuery
-- DNSMessage {identifier = 0, opcode = OP_STD, rcode = NoError, flags = DNSFlags {isResponse = False, authAnswer = False, trunCation = False, recDesired = True, recAvailable = False, authenData = False, chkDisable = False}, ednsHeader = EDNSheader (EDNS {ednsVersion = 0, ednsUdpSize = 1232, ednsDnssecOk = False, ednsOptions = []}), question = Question {qname = ".", qtype = A, qclass = IN}, answer = [], authority = [], additional = []}
defaultQuery :: DNSMessage
defaultQuery =
    DNSMessage
        { identifier = 0
        , opcode = OP_STD
        , rcode = NoErr
        , flags = defaultQueryDNSFlags
        , ednsHeader = EDNSheader defaultEDNS
        , question = defaultQuestion
        , answer = []
        , authority = []
        , additional = []
        }

-- | Construct a complete query 'DNSMessage', by combining the 'defaultQuery'
-- template with the specified 'Identifier', and 'Question'.
makeQuery
    :: Identifier
    -- ^ Crypto random request id
    -> Question
    -- ^ Question name and type
    -> DNSMessage
makeQuery idt q =
    defaultQuery
        { identifier = idt
        , question = q
        }

-- | Default response.  When responding to EDNS queries, the response must
-- either be an EDNS response, or else FormatErr must be returned.  The default
-- response message has EDNS disabled ('ednsHeader' set to 'NoEDNS'), it should
-- be updated as appropriate.
--
-- Do not explicitly add OPT RRs to the additional section, instead let the
-- encoder compute and add the OPT record based on the EDNS pseudo-header.
--
-- The 'RCODE' in the 'DNSHeader' should be set to the appropriate 12-bit
-- extended value, which will be split between the primary header and EDNS OPT
-- record during message encoding (low 4 bits in DNS header, high 8 bits in
-- EDNS OPT record).  See 'EDNSheader' for more details.
--
-- >>> defaultResponse
-- DNSMessage {identifier = 0, opcode = OP_STD, rcode = NoError, flags = DNSFlags {isResponse = True, authAnswer = True, trunCation = False, recDesired = True, recAvailable = True, authenData = False, chkDisable = False}, ednsHeader = NoEDNS, question = Question {qname = ".", qtype = A, qclass = IN}, answer = [], authority = [], additional = []}
defaultResponse :: DNSMessage
defaultResponse =
    DNSMessage
        { identifier = 0
        , opcode = OP_STD
        , rcode = NoErr
        , flags = defaultResponseDNSFlags
        , ednsHeader = NoEDNS
        , question = defaultQuestion
        , answer = []
        , authority = []
        , additional = []
        }

-- | Construct a response 'DNSMessage'.
makeResponse
    :: Identifier
    -> Question
    -> Answers
    -> DNSMessage
makeResponse idt q as =
    defaultResponse
        { identifier = idt
        , question = q
        , answer = as
        }

----------------------------------------------------------------

-- | Raw data format for the flags of DNS Query and Response.
data DNSFlags = DNSFlags
    { isResponse :: Bool
    -- ^ QR (Queary or Response) bit - this bit is set if the message is response.
    , authAnswer :: Bool
    -- ^ AA (Authoritative Answer) bit - this bit is valid in responses,
    -- and specifies that the responding name server is an
    -- authority for the domain name in question section.
    , trunCation :: Bool
    -- ^ TC (Truncated Response) bit - specifies that this message was truncated
    -- due to length greater than that permitted on the
    -- transmission channel.
    , recDesired :: Bool
    -- ^ RD (Recursion Desired) bit - this bit may be set in a query and
    -- is copied into the response.  If RD is set, it directs
    -- the name server to pursue the query recursively.
    -- Recursive query support is optional.
    , recAvailable :: Bool
    -- ^ RA (Recursion Available) bit - this be is set or cleared in a
    -- response, and denotes whether recursive query support is
    -- available in the name server.
    , authenData :: Bool
    -- ^ AD (Authenticated Data) bit - (RFC4035, Section 3.2.3).
    , chkDisable :: Bool
    -- ^ CD (Checking Disabled) bit - (RFC4035, Section 3.2.2).
    }
    deriving (Eq, Show)

----------------------------------------------------------------

-- | Default query 'DNSFlags' record suitable for making recursive queries.
--   By default the RD bit is set, and the AD and CD bits are cleared.
--
-- >>> defaultQueryDNSFlags
-- DNSFlags {isResponse = False, authAnswer = False, trunCation = False, recDesired = True, recAvailable = False, authenData = False, chkDisable = False}
defaultQueryDNSFlags :: DNSFlags
defaultQueryDNSFlags =
    DNSFlags
        { isResponse = False
        , authAnswer = False
        , trunCation = False
        , recDesired = True
        , recAvailable = False
        , authenData = False
        , chkDisable = False
        }

{-# DEPRECATED defaultDNSFlags "use defaultQueryDNSFlags" #-}
defaultDNSFlags :: DNSFlags
defaultDNSFlags = defaultQueryDNSFlags

defaultResponseDNSFlags :: DNSFlags
defaultResponseDNSFlags =
    DNSFlags
        { isResponse = True
        , authAnswer = True
        , trunCation = False
        , recDesired = True
        , recAvailable = True
        , authenData = False
        , chkDisable = False
        }

putDNSFlags :: (DNSFlags, OPCODE, RCODE) -> Builder ()
putDNSFlags (DNSFlags{..}, op, rc) wbuf _ = put16 wbuf word
  where
    set :: Word16 -> State Word16 ()
    set byte = ST.modify (.|. byte)

    st :: State Word16 ()
    st =
        sequence_
            [ set (fromRCODE rc .&. 0x0f)
            , when chkDisable $ set (bit 4)
            , when authenData $ set (bit 5)
            , when recAvailable $ set (bit 7)
            , when recDesired $ set (bit 8)
            , when trunCation $ set (bit 9)
            , when authAnswer $ set (bit 10)
            , set (fromOPCODE op `shiftL` 11)
            , when isResponse $ set (bit 15)
            ]

    word = ST.execState st 0

getDNSFlags :: Parser (DNSFlags, OPCODE, RCODE)
getDNSFlags rbuf _ = do
    flgs <- get16 rbuf
    let op = getOpcode flgs
        rc = getRcode flgs
        flg =
            DNSFlags
                (getIsResponse flgs)
                (getAuthAnswer flgs)
                (getTrunCation flgs)
                (getRecDesired flgs)
                (getRecAvailable flgs)
                (getAuthenData flgs)
                (getChkDisable flgs)
    return (flg, op, rc)
  where
    getIsResponse w = testBit w 15
    getOpcode w = toOPCODE (shiftR w 11 .&. 0x0f)
    getAuthAnswer w = testBit w 10
    getTrunCation w = testBit w 9
    getRecDesired w = testBit w 8
    getRecAvailable w = testBit w 7
    getRcode w = toRCODE $ w .&. 0x0f
    getAuthenData w = testBit w 5
    getChkDisable w = testBit w 4

----------------------------------------------------------------

-- | Kind of query.
newtype OPCODE = OPCODE
    { fromOPCODE :: Word16
    -- ^ Convert an 'OPCODE' to its numeric value.
    }
    deriving (Eq)

-- | A standard query.
pattern OP_STD :: OPCODE
pattern OP_STD = OPCODE 0

-- | An inverse query (inverse queries are deprecated).
pattern OP_INV :: OPCODE
pattern OP_INV = OPCODE 1

-- | A server status request.
pattern OP_SSR :: OPCODE
pattern OP_SSR = OPCODE 2

-- OPCODE 3 is not assigned

-- | A zone change notification (RFC1996)
pattern OP_NOTIFY :: OPCODE
pattern OP_NOTIFY = OPCODE 4

-- | An update request (RFC2136)
pattern OP_UPDATE :: OPCODE
pattern OP_UPDATE = OPCODE 5

-- | Convert a 16-bit DNS OPCODE number to its internal representation
toOPCODE :: Word16 -> OPCODE
toOPCODE = OPCODE

instance Show OPCODE where
    show OP_STD = "OP_STD"
    show OP_INV = "OP_INV"
    show OP_SSR = "OP_SSR"
    show OP_NOTIFY = "OP_NOTIFY"
    show OP_UPDATE = "OP_UPDATE"
    show (OPCODE n) = "OPCODE " ++ show n

----------------------------------------------------------------

-- | EDNS extended 12-bit response code.  Non-EDNS messages use only the low 4
-- bits.  With EDNS this stores the combined error code from the DNS header and
-- and the EDNS psuedo-header. See 'EDNSheader' for more detail.
newtype RCODE = RCODE
    { fromRCODE :: Word16
    -- ^ Convert an 'RCODE' to its numeric value.
    }
    deriving (Eq, Ord)

-- | Provide an Enum instance for backwards compatibility
instance Enum RCODE where
    fromEnum = fromIntegral . fromRCODE
    toEnum = RCODE . fromIntegral

-- | No error condition.
pattern NoErr :: RCODE
pattern NoErr = RCODE 0

-- | Format error - The name server was
--   unable to interpret the query.
pattern FormatErr :: RCODE
pattern FormatErr = RCODE 1

-- | Server failure - The name server was
--   unable to process this query due to a
--   problem with the name server.
pattern ServFail :: RCODE
pattern ServFail = RCODE 2

-- | Non-Existent Domain - Meaningful only for
--   responses from an authoritative name
--   server, this code signifies that the
--   domain name referenced in the query does
--   not exist.
pattern NXDomain :: RCODE
pattern NXDomain = RCODE 3

-- | Name Error
--   NXDOMAIN was introduced in RFC 2308 as an alternative expression
--   for 'Name Error' defined in RFC 1035.
--   Since many RFCs still use the 'Name Error' expression in their
--   descriptions,  we'll keep the alias for clarity.
pattern NameErr :: RCODE
pattern NameErr = NXDomain

-- | Not Implemented - The name server does
--   not support the requested kind of query.
pattern NotImpl :: RCODE
pattern NotImpl = RCODE 4

-- | Refused - The name server refuses to
--   perform the specified operation for
--   policy reasons.  For example, a name
--   server may not wish to provide the
--   information to the particular requester,
--   or a name server may not wish to perform
--   a particular operation (e.g., zone
--   transfer) for particular data.
pattern Refused :: RCODE
pattern Refused = RCODE 5

-- | YXDomain - Dynamic update response, a pre-requisite domain that should not
-- exist, does exist.
pattern YXDomain :: RCODE
pattern YXDomain = RCODE 6

-- | YXRRSet - Dynamic update response, a pre-requisite RRSet that should not
-- exist, does exist.
pattern YXRRSet :: RCODE
pattern YXRRSet = RCODE 7

-- | NXRRSet - Dynamic update response, a pre-requisite RRSet that should
-- exist, does not exist.
pattern NXRRSet :: RCODE
pattern NXRRSet = RCODE 8

-- | NotAuth - Dynamic update response, the server is not authoritative for the
-- zone named in the Zone Section.
pattern NotAuth :: RCODE
pattern NotAuth = RCODE 9

-- | NotZone - Dynamic update response, a name used in the Prerequisite or
-- Update Section is not within the zone denoted by the Zone Section.
pattern NotZone :: RCODE
pattern NotZone = RCODE 10

-- | Bad OPT Version (BADVERS, RFC 6891).
pattern BadVers :: RCODE
pattern BadVers = RCODE 16

-- | Key not recognized [RFC2845]
pattern BadKey :: RCODE
pattern BadKey = RCODE 17

-- | Signature out of time window [RFC2845]
pattern BadTime :: RCODE
pattern BadTime = RCODE 18

-- | Bad TKEY Mode [RFC2930]
pattern BadMode :: RCODE
pattern BadMode = RCODE 19

-- | Duplicate key name [RFC2930]
pattern BadName :: RCODE
pattern BadName = RCODE 20

-- | Algorithm not supported [RFC2930]
pattern BadAlg :: RCODE
pattern BadAlg = RCODE 21

-- | Bad Truncation [RFC4635]
pattern BadTrunc :: RCODE
pattern BadTrunc = RCODE 22

-- | Bad/missing Server Cookie [RFC7873]
pattern BadCookie :: RCODE
pattern BadCookie = RCODE 23

-- | Malformed (peer) EDNS message, no RCODE available.  This is not an RCODE
-- that can be sent by a peer.  It lies outside the 12-bit range expressible
-- via EDNS.  The low 12-bits are chosen to coincide with 'FormatErr'.  When
-- an EDNS message is malformed, and we're unable to extract the extended RCODE,
-- the header 'rcode' is set to 'BadRCODE'.
pattern BadRCODE :: RCODE
pattern BadRCODE = RCODE 0x1001

-- | Use https://tools.ietf.org/html/rfc2929#section-2.3 names for DNS RCODEs
instance Show RCODE where
    show NoErr = "NoError"
    show FormatErr = "FormErr"
    show ServFail = "ServFail"
    show NXDomain = "NXDomain"
    show NotImpl = "NotImp"
    show Refused = "Refused"
    show YXDomain = "YXDomain"
    show YXRRSet = "YXRRSet"
    show NotAuth = "NotAuth"
    show NotZone = "NotZone"
    show BadVers = "BadVers"
    show BadKey = "BadKey"
    show BadTime = "BadTime"
    show BadMode = "BadMode"
    show BadName = "BadName"
    show BadAlg = "BadAlg"
    show BadTrunc = "BadTrunc"
    show BadCookie = "BadCookie"
    show (RCODE n) = "RCODE " ++ show n

-- | Convert a numeric value to a corresponding 'RCODE'.  The behaviour is
-- undefined for values outside the range @[0 .. 0xFFF]@ since the EDNS
-- extended RCODE is a 12-bit value.  Values in the range @[0xF01 .. 0xFFF]@
-- are reserved for private use.
toRCODE :: Word16 -> RCODE
toRCODE = RCODE

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Raw data format for DNS questions.
data Question = Question
    { qname  :: Domain
    -- ^ A domain name
    , qtype  :: TYPE
    -- ^ The type of the query
    , qclass :: CLASS
    }
    deriving (Eq, Ord, Show)
{- FOURMOLU_ENABLE -}

defaultQuestion :: Question
defaultQuestion =
    Question
        { qname = "."
        , qtype = A
        , qclass = IN
        }

putQuestion :: CanonicalFlag -> Question -> Builder ()
putQuestion cf Question{..} wbuf ref = do
    putDomainRFC1035 cf qname wbuf ref
    put16 wbuf $ fromTYPE qtype
    putCLASS qclass wbuf ref

{- FOURMOLU_DISABLE -}
getQuestion :: Parser Question
getQuestion rbuf ref = do
    qname  <- getDomainRFC1035 rbuf ref
    qtype  <- getTYPE rbuf ref
    qclass <- getCLASS rbuf ref
    return Question{..}
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | Resource record class.
newtype CLASS = CLASS
    { fromCLASS :: Word16
    -- ^ Convert an 'CLASS' to its numeric value.
    }
    deriving (Eq, Ord)

toCLASS :: Word16 -> CLASS
toCLASS = CLASS

-- | Resource record class for the Internet.
pattern IN :: CLASS
pattern IN = CLASS 1

-- | Resource record class for Chaos
pattern CH :: CLASS
pattern CH = CLASS 3

instance Show CLASS where
    show IN = "IN"
    show CH = "CH"
    show (CLASS n) = "CLASS " ++ show n

putCLASS :: CLASS -> Builder ()
putCLASS (CLASS x) wbuf _ = put16 wbuf x

getCLASS :: Parser CLASS
getCLASS rbuf _ = CLASS <$> get16 rbuf

-- | Time to live in second.
type TTL = Seconds

{- FOURMOLU_DISABLE -}
-- | Raw data format for resource records.
data ResourceRecord = ResourceRecord
    { rrname  :: Domain
    -- ^ Name
    , rrtype  :: TYPE
    -- ^ Resource record type
    , rrclass :: CLASS
    -- ^ Resource record class
    , rrttl   :: TTL
    -- ^ Time to live
    , rdata   :: RData
    -- ^ Resource data
    }
    deriving (Eq, Show)
{- FOURMOLU_ENABLE -}

resourceRecordSize :: ResourceRecord -> Int
resourceRecordSize ResourceRecord{..} = domainSize rrname + 10 + rdataSize rdata

-- | Type alias for resource records in the answer section.
type Answers = [ResourceRecord]

-- | Type alias for resource records in the answer section.
type AuthorityRecords = [ResourceRecord]

-- | Type for resource records in the additional section.
type AdditionalRecords = [ResourceRecord]

putResourceRecord :: CanonicalFlag -> ResourceRecord -> Builder ()
putResourceRecord cf ResourceRecord{..} wbuf ref = do
    putDomainRFC1035 cf rrname wbuf ref
    putTYPE rrtype wbuf ref
    putCLASS rrclass wbuf ref
    putSeconds rrttl wbuf ref
    with16Length (putRData cf rdata) wbuf ref

getResourceRecords :: Int -> Parser [ResourceRecord]
getResourceRecords n rbuf ref = go 0 id
  where
    go i b
        | i == n = return $ b []
        | otherwise = do
            r <- getResourceRecord rbuf ref
            if rrtype r == TYPE 0 && rrclass r == CLASS 0 -- skipping greasing RR
                then go i b
                else go (i + 1) (b . (r :))

{- FOURMOLU_DISABLE -}
getResourceRecord :: Parser ResourceRecord
getResourceRecord rbuf ref = do
    rrname  <- getDomainRFC1035 rbuf ref
    rrtype  <- getTYPE rbuf ref
    rrclass <- getCLASS rbuf ref
    rrttl   <- getSeconds rbuf ref
    len     <- getInt16 rbuf
    rdata   <- getRData rrtype len rbuf ref
    return ResourceRecord {..}
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

data Section = Answer | Authority | Additional deriving (Eq, Ord, Show)

section :: Section -> DNSMessage -> Answers
section Answer = answer
section Authority = authority
section Additional = additional

extractResourceData :: ResourceData a => Section -> DNSMessage -> [a]
extractResourceData sec = mapMaybe (fromRData . rdata) . section sec

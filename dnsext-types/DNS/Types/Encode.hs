{-# LANGUAGE RecordWildCards #-}

module DNS.Types.Encode (
    -- * Main encoder
    encode,

    -- * Encoders for parts
    encodeDNSFlags,
    encodeQuestion,
    encodeResourceRecord,
    encodeRData,
    encodeDomain,
    encodeMailbox,
) where

import DNS.Types.Domain
import DNS.Types.EDNS
import DNS.Types.Imports
import DNS.Types.Message
import DNS.Types.RData
import DNS.Wire

-- | Encode DNS message.
encode :: DNSMessage -> ByteString
encode msg@DNSMessage{..} = runBuilder siz $ putDNSMessage msg
  where
    siz =
        16
            + mapEDNS ednsHeader ednsSize 0
            + qsiz question
            + sum (map resourceRecordSize answer)
            + sum (map resourceRecordSize authority)
            + sum (map resourceRecordSize additional)
    ednsSize eh = 11 + sum (map (\o -> 4 + odataSize o) $ ednsOptions eh)

-- | Encode DNS flags.
encodeDNSFlags :: (DNSFlags, OPCODE, RCODE) -> ByteString
encodeDNSFlags = runBuilder 2 . putDNSFlags

-- | Encode a question.
encodeQuestion :: Question -> ByteString
encodeQuestion q = runBuilder (qsiz q) $ putQuestion Original q

qsiz :: Question -> Int
qsiz Question{..} = domainSize qname + 4

-- | Encode a resource record.
encodeResourceRecord :: ResourceRecord -> ByteString
encodeResourceRecord rr = runBuilder (resourceRecordSize rr) $ putResourceRecord Original rr

-- | Encode a resource data.
encodeRData :: RData -> ByteString
encodeRData rd = runBuilder (rdataSize rd) $ putRData Original rd

-- | Encode a domain with name compression.
encodeDomain :: Domain -> ByteString
encodeDomain d = runBuilder (domainSize d) $ putDomainRFC1035 Original d

-- | Encode a mailbox name with name compression.
encodeMailbox :: Mailbox -> ByteString
encodeMailbox m = runBuilder (mailboxSize m) $ putMailboxRFC1035 Original m

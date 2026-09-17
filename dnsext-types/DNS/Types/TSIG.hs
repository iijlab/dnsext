{-# LANGUAGE RecordWildCards #-}

-- | Assembling the octets a TSIG MAC is computed over (RFC 8945).
--
--   No secret and no hashing here: this is the wire format side of TSIG
--   alone, which is where the mistakes are made.  What to do with the
--   octets is somebody else's business.
module DNS.Types.TSIG (
    tsigDigest,
    tsigDigestCont,
    tsigMacField,
    tsigVariables,
    tsigTimers,
) where

import qualified Data.ByteString as BS

import DNS.Types.Domain
import DNS.Types.Imports
import DNS.Types.Message
import DNS.Types.Opaque.Internal (Opaque, putOpaque)
import qualified DNS.Types.Opaque.Internal as Opaque
import DNS.Types.RData
import DNS.Wire

----------------------------------------------------------------

-- | The octets a TSIG MAC is computed over, for a request or for the
--   first message of a response (RFC 8945 Sec 4.3): the MAC of the
--   request if this is a response, the message, then the TSIG
--   variables.
--
--   The message must be given exactly as it goes on the wire, before
--   the TSIG record is added to it and before ARCOUNT counts that
--   record -- and, when verifying, exactly as it arrived.  Encoding it
--   afresh will not do: an encoder is free to compress names as it
--   likes, and the MAC covers the octets rather than the message they
--   happen to spell.
tsigDigest
    :: Maybe Opaque
    -- ^ MAC of the request, when signing or checking a response
    -> ByteString
    -- ^ the message, in wire format, without its TSIG record
    -> Domain
    -- ^ name of the key
    -> RD_TSIG
    -> ByteString
tsigDigest mrequest body keyname tsig =
    maybe BS.empty tsigMacField mrequest <> body <> tsigVariables keyname tsig

-- | The octets for a message after the first one of a multi-message
--   response (RFC 8945 Sec 5.3.1): the MAC of the last message which
--   carried a TSIG, every message since then, then the timers alone.
tsigDigestCont
    :: Opaque
    -- ^ MAC of the last message which carried a TSIG
    -> [ByteString]
    -- ^ the messages since then in wire format, this one last
    -> RD_TSIG
    -- ^ only 'tsig_time_signed' and 'tsig_fudge' are taken from this
    -> ByteString
tsigDigestCont prior bodies tsig =
    tsigMacField prior <> BS.concat bodies <> tsigTimers tsig

----------------------------------------------------------------

-- | A MAC as it enters a digest (RFC 8945 Sec 4.3.1): its length as an
--   unsigned 16 bit integer, then its octets.
tsigMacField :: Opaque -> ByteString
tsigMacField mac = runBuilder (2 + len) $ \wbuf ref -> do
    put16 wbuf $ fromIntegral len
    putOpaque mac wbuf ref
  where
    len = Opaque.length mac

-- | The TSIG variables (RFC 8945 Sec 4.3.3): the fields of the record
--   which are covered but do not travel in the message body.
--
--   Both names go in canonical wire format, which 'putDomain' gives --
--   never compressed, and lower case because that is how a 'Domain'
--   keeps its labels.  The class and the TTL are written as the values
--   the record is required to carry rather than read from anywhere,
--   since a TSIG with any others is not a TSIG.
tsigVariables :: Domain -> RD_TSIG -> ByteString
tsigVariables keyname RD_TSIG{..} = runBuilder siz $ \wbuf ref -> do
    putDomain Canonical keyname wbuf ref
    putCLASS CL_ANY wbuf ref
    put32 wbuf 0 -- TTL
    putDomain Canonical tsig_algorithm wbuf ref
    put48 wbuf tsig_time_signed
    put16 wbuf tsig_fudge
    put16 wbuf tsig_error
    put16 wbuf $ fromIntegral otherlen
    putOpaque tsig_other wbuf ref
  where
    otherlen = Opaque.length tsig_other
    -- name + class + ttl + algorithm + time + fudge + error + len
    siz = domainSize keyname + 6 + domainSize tsig_algorithm + 12 + otherlen

-- | The TSIG timers (RFC 8945 Sec 5.3.1): the time and the fudge, in
--   that order, and nothing else.
tsigTimers :: RD_TSIG -> ByteString
tsigTimers RD_TSIG{..} = runBuilder 8 $ \wbuf _ -> do
    put48 wbuf tsig_time_signed
    put16 wbuf tsig_fudge

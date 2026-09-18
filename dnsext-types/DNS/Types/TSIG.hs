{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    stripTSIG,
    unsignedTSIG,
) where

import qualified Data.ByteString as BS

import DNS.Types.Domain
import DNS.Types.Imports
import DNS.Types.Message
import DNS.Types.Opaque.Internal (Opaque, putOpaque)

import qualified DNS.Types.Opaque.Internal as Opaque
import DNS.Types.RData
import DNS.Types.Type
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

-- | A record with no MAC in it yet: everything a MAC is taken over,
--   before there is one to take.  Put the MAC in afterwards with
--   @rd{tsig_mac = mac}@; nothing covers itself.
--
--   The error is zero and there is no other data, which is what a
--   record made to sign with carries.  A record made to say what went
--   wrong is a different thing and is built where that is decided.
unsignedTSIG
    :: Domain
    -- ^ name of the algorithm
    -> Word64
    -- ^ time signed
    -> Word16
    -- ^ fudge
    -> Word16
    -- ^ identifier of the message being signed
    -> RD_TSIG
unsignedTSIG alg time fudge origid =
    RD_TSIG
        { tsig_algorithm = alg
        , tsig_time_signed = time
        , tsig_fudge = fudge
        , tsig_mac = Opaque.fromByteString ""
        , tsig_original_id = origid
        , tsig_error = 0
        , tsig_other = Opaque.fromByteString ""
        }

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

----------------------------------------------------------------

-- | The octets of a received message up to its TSIG record, with
--   ARCOUNT put back to what it was before the record was added -- in
--   other words, what 'tsigDigest' wants for the message.
--
--   Cut out of the message as it arrived rather than got by encoding a
--   decoded one again.  A MAC covers the octets, and an encoder is free
--   to compress names as it likes, so a message which went out one way
--   can come back from the decoder and go out another, with the same
--   DNS content and a different MAC.
--
--   'Nothing' unless the message really does end in a TSIG record, as
--   RFC 8945 Sec 5.1 requires of one, and unless every name and length
--   on the way to it stays inside the message.  'Nothing' as well for a
--   message carrying a TSIG anywhere else, or more than one of them,
--   which Sec 5.2 has dropped and answered FORMERR rather than checked:
--   it says what the one record covers, and two of them say nothing at
--   all.  The contents of the record come from the decoder as usual;
--   only the octets are wanted here.
stripTSIG :: ByteString -> Maybe ByteString
stripTSIG bs = do
    guard $ BS.length bs >= 12
    qd <- word16At 4
    an <- word16At 6
    ns <- word16At 8
    ar <- word16At 10
    guard $ ar >= 1
    afterQs <- foldM (\i _ -> skipQuestion i) 12 [1 .. qd]
    -- Every record but the last one, which is the one we are after.
    -- Sec 5.2: exactly one TSIG, so one before the end is as bad as
    -- none at the end.
    at <- foldM (\i _ -> skipOrdinary i) afterQs [1 .. an + ns + ar - 1]
    typeAt <- skipName at
    typ <- word16At typeAt
    guard $ typ == tsigType
    -- Sec 5.1: the TSIG is the last record there is.
    end <- skipRecord at
    guard $ end == BS.length bs
    return $ setARCOUNT (ar - 1) $ BS.take at bs
  where
    tsigType = fromIntegral $ fromTYPE TSIG
    len = BS.length bs
    octet :: Int -> Maybe Word8
    octet i = if 0 <= i && i < len then Just (BS.index bs i) else Nothing
    word16At :: Int -> Maybe Int
    word16At i = do
        hi <- octet i
        lo <- octet (i + 1)
        return $ fromIntegral hi * 256 + fromIntegral lo
    -- A name is labels until a zero octet, or a pointer, which ends it.
    skipName :: Int -> Maybe Int
    skipName = go
      where
        go i = do
            w <- octet i
            if
                | w == 0 -> Just (i + 1)
                | w >= 0xc0 -> fitting (i + 2)
                | w < 0x40 -> go (i + 1 + fromIntegral w)
                -- 0x40 to 0xbf is not ours to guess at
                | otherwise -> Nothing
    skipQuestion :: Int -> Maybe Int
    skipQuestion i = skipName i >>= \j -> fitting (j + 4)
    skipRecord :: Int -> Maybe Int
    skipRecord i = do
        j <- skipName i
        -- type, class, TTL, then the length of what follows
        rdlen <- word16At (j + 8)
        fitting (j + 10 + rdlen)
    -- A record which is not a TSIG, since a TSIG here is not ours to
    -- take.
    skipOrdinary :: Int -> Maybe Int
    skipOrdinary i = do
        j <- skipName i
        typ <- word16At j
        guard $ typ /= tsigType
        skipRecord i
    fitting :: Int -> Maybe Int
    fitting i = if i <= len then Just i else Nothing
    setARCOUNT :: Int -> ByteString -> ByteString
    setARCOUNT n b =
        BS.concat
            [ BS.take 10 b
            , BS.pack [fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
            , BS.drop 12 b
            ]

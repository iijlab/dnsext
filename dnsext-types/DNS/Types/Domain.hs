{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Types.Domain (
    IsRepresentation (..),
    Domain,
    superDomains',
    superDomains,
    leafDomain,
    unsafeLeafDomain,
    isSubDomainOf,
    labelsCount,
    domainSize,
    consDomain,
    unconsDomain,
    wireLabels_,
    wireLabels,
    revLabels,
    Mailbox,
    mailboxSize,
    getDomain,
    getDomainRFC1035,
    putDomain,
    putDomainRFC1035,
    putMailbox,
    putMailboxRFC1035,
    getMailbox,
    getMailboxRFC1035,
    CanonicalFlag (..),
) where

import qualified Control.Exception as E
import Data.Array.Base (listArray, (!))
import qualified Data.Array.Base as Array
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import Data.Functor (($>))
import Data.Word8

import DNS.Types.Error
import DNS.Types.Imports
import qualified DNS.Types.Parser as P
import DNS.Wire

-- $setup
-- >>> :seti -XOverloadedStrings

class IsRepresentation a b where
    fromRepresentation :: b -> a
    toRepresentation :: a -> b
    fromWireLabels :: [b] -> a
    toWireLabels :: a -> [b]

-- | The type for domain names. This holds the /wire format/ internally.
--
-- The representation format is fully-qualified DNS domain names encoded
-- as ASCII A-labels, with \'.\' separators between labels.
-- The trailing \'.\' is added if missing.
-- Non-printing characters are escaped as @\\DDD@ (a backslash,
-- followed by three decimal digits). The special characters: @ \",
-- \$, (, ), ;, \@,@ and @\\@ are escaped by prepending a backslash.
--
-- The representation format is ASCII-only. Any conversion between
-- A-label 'Text's, and U-label 'Text' happens at whatever layer maps
-- user input to DNS names, or presents /friendly/ DNS names to the
-- user.  Not all users can read all scripts, and applications that
-- default to U-label format should ideally give the user a choice to
-- see the A-label format.  Examples:
--
-- A 'IllegalDomain' may be thrown when creating from the representation
-- format or decoding the wire format.
--
-- >>> "" :: Domain
-- "."
-- >>> "." :: Domain
-- "."
-- >>> wireLabels "."
-- []
-- >>> "www.example.org" :: Domain
-- "www.example.org."
-- >>> "www.example.org." :: Domain
-- "www.example.org."
-- >>> "_25._tcp.mx1.example.net." :: Domain
-- "_25._tcp.mx1.example.net."
-- >>> wireLabels "_25._tcp.mx1.example.net."
-- ["_25","_tcp","mx1","example","net"]
-- >>> "\\001.exotic.example." :: Domain -- fixme
-- "\001.exotic.example."
-- >>> wireLabels "\\001.exotic.example."
-- ["\SOH","exotic","example"]
-- >>> wireLabels "just\\.one\\.label.example."
-- ["just.one.label","example"]
newtype Domain = Domain
    { wireLabels_ :: WireLabels
    -- ^ Labels in wire format. Lower cases, not escaped.
    --   https://datatracker.ietf.org/doc/html/rfc4034#section-6.1
    }

rootDomain :: Domain
rootDomain = Domain emptyLabels

listWireLabels :: [Label] -> WireLabels
listWireLabels xs = listArray (0, length xs - 1) xs

wireLabels :: Domain -> [Label]
wireLabels = Array.elems . wireLabels_

emptyLabels :: WireLabels
emptyLabels = listWireLabels []

domain :: ShortByteString -> Domain
domain "" = rootDomain
domain "." = rootDomain
domain o =
    validateDomain $
        Domain
            { wireLabels_ = ls
            }
  where
    ls = listWireLabels $ unfoldr step $ lowercase o
    step x = case parseLabel _period x of
        Nothing -> Nothing
        just@(Just (p, _))
            | p == "" -> Nothing
            | otherwise -> just

domainFromWireLabels :: WireLabels -> Domain
domainFromWireLabels = Domain

instance Eq Domain where
    Domain d0 == Domain d1 = d0 `eqF` d1

eqF :: WireLabels -> WireLabels -> Bool
eqF v0 v1 = l0 == l1 && go 0
  where
    l0 = Array.numElements v0
    l1 = Array.numElements v1
    go i
        | i == l0 = True
        | otherwise =
            v0 ! i == v1 ! i
                && go (i + 1)

-- | Ordering according to the DNSSEC definition.
--
-- >>> ("www.example.jp" :: Domain) >= "example.jp"
-- True
-- >>> ("example8.jp" :: Domain) >= "example.jp"
-- True
-- >>> ("example.jp" :: Domain) >= "example.com"
-- True
instance Ord Domain where
    Domain d0 `compare` Domain d1 = d0 `cmpR` d1

cmpR :: WireLabels -> WireLabels -> Ordering
cmpR v0 v1 = go (l0 - 1) (l1 - 1)
  where
    l0 = Array.numElements v0
    l1 = Array.numElements v1
    go (-1) (-1) = EQ
    go (-1) _ = LT
    go _ (-1) = GT
    go i j =
        let e0 = v0 ! i
            e1 = v1 ! j
         in case e0 `compare` e1 of
                EQ -> go (i - 1) (j - 1)
                LT -> LT
                GT -> GT

instance Show Domain where
    show d = "\"" ++ shortToString (toDomainRep d) ++ "\""

toDomainRep :: Domain -> Label
toDomainRep (Domain d)
    | d == emptyLabels = "."
    | otherwise = foldr (\l r -> escapeLabel _period l <> "." <> r) "" d

instance IsString Domain where
    fromString = fromRepresentation

-- | Appending two domains.
--
-- >>> ("www" :: Domain) <> "example.com"
-- "www.example.com."
-- >>> ("www." :: Domain) <> "example.com."
-- "www.example.com."
instance Semigroup Domain where
    d0 <> d1 = domainFromWireLabels (listWireLabels $ wireLabels d0 <> wireLabels d1)

instance IsRepresentation Domain ShortByteString where
    fromRepresentation = domain
    toRepresentation = toDomainRep
    fromWireLabels = domainFromWireLabels . listWireLabels
    toWireLabels = wireLabels

instance IsRepresentation Domain ByteString where
    fromRepresentation = domain . Short.toShort
    toRepresentation = Short.fromShort . toDomainRep
    fromWireLabels = domainFromWireLabels . listWireLabels . (Short.toShort <$>)
    toWireLabels = (Short.fromShort <$>) . wireLabels

instance IsRepresentation Domain String where
    fromRepresentation = domain . fromString
    toRepresentation = shortToString . toDomainRep
    fromWireLabels = domainFromWireLabels . listWireLabels . (fromString <$>)
    toWireLabels = (shortToString <$>) . wireLabels

-- | Wire size of domain.
--
-- >>> domainSize "."
-- 1
-- >>> domainSize "jp"
-- 4
-- >>> domainSize "example.jp"
-- 12
domainSize :: Domain -> Int
domainSize (Domain d) = foldr (\l a -> Short.length l + 1 + a) 0 d + 1

consDomain :: Label -> Domain -> Domain
consDomain l dom = fromWireLabels (lowercase l : wireLabels dom)

-- | Uncos a domain
--
-- >>> unconsDomain "."
-- Nothing
-- >>> unconsDomain "jp"
-- Just ("jp",".")
-- >>> unconsDomain "example.jp."
-- Just ("example","jp.")
unconsDomain :: Domain -> Maybe (Label, Domain)
unconsDomain (Domain d) = case uncons (Array.elems d) of
    Nothing -> Nothing
    Just (l, d') -> Just (l, Domain $ listWireLabels d')

-- | Getting the left most label of a domain.
--   'Nothing' for the root domain, which has no label.
--
-- >>> leafDomain "www.example.jp."
-- Just "www"
-- >>> leafDomain "example.jp."
-- Just "example"
-- >>> leafDomain "jp."
-- Just "jp"
-- >>> leafDomain "*.example.jp."
-- Just "*"
-- >>> leafDomain "."
-- Nothing
leafDomain :: Domain -> Maybe Label
leafDomain (Domain d)
    | Array.numElements d == 0 = Nothing
    | otherwise = Just (d ! 0)

-- | Getting the left most label of a domain.
--   \".\" for the root domain, which has no label.
--
-- >>> unsafeLeafDomain "www.example.jp."
-- "www"
-- >>> unsafeLeafDomain "jp."
-- "jp"
-- >>> unsafeLeafDomain "."
-- "."
unsafeLeafDomain :: Domain -> Label
unsafeLeafDomain (Domain d)
    | Array.numElements d == 0 = "."
    | otherwise = d ! 0

-- | Generating a reverse list of domain labels.
--
-- >>> revLabels "www.example.jp"
-- ["jp","example","www"]
-- >>> revLabels "."
-- []
revLabels :: Domain -> [Label]
revLabels (Domain d) = [d ! i | i <- [sz - 1, sz - 2 .. 0]]
  where
    sz = Array.numElements d

----------------------------------------------------------------

validateDomain :: Domain -> Domain
validateDomain d
    | isIllegal (wireLabels_ d) = E.throw IllegalDomain
    | otherwise = d

validateMailbox :: Mailbox -> Mailbox
validateMailbox m@(Mailbox d)
    | isIllegal (wireLabels_ d) = E.throw IllegalDomain
    | otherwise = m

isIllegal :: WireLabels -> Bool
isIllegal ls = sum is > 255 || any (> 63) is
  where
    is = foldr (\x -> (Short.length x :)) [] ls

----------------------------------------------------------------

-- | The type for mailbox whose internal is just 'Domain'.
--   The representation format must include \'@\'.
-- Examples:
--
-- @
-- hostmaster\@example.org.  -- First label is simply @hostmaster@
-- john.smith\@examle.com.   -- First label is @john.smith@
-- @
newtype Mailbox = Mailbox {fromMailbox :: Domain}

toMailbox :: Domain -> Mailbox
toMailbox = Mailbox

instance Eq Mailbox where
    Mailbox d0 == Mailbox d1 = d0 == d1

instance Ord Mailbox where
    Mailbox d0 <= Mailbox d1 = d0 <= d1

instance Show Mailbox where
    show mbox = "\"" ++ shortToString (toMailboxRep mbox) ++ "\""

toMailboxRep :: Mailbox -> Label
toMailboxRep (Mailbox (Domain d)) = case uncons (Array.elems d) of
    Nothing -> E.throw IllegalDomain
    Just (name, d') -> name <> "@" <> foldr (\x y -> escapeLabel _period x <> "." <> y) "" d'

instance IsString Mailbox where
    fromString = fromRepresentation

instance Semigroup Mailbox where
    Mailbox d0 <> Mailbox d1 = Mailbox (d0 <> d1)

mailbox :: ShortByteString -> Mailbox
mailbox o
    | Short.length o > 255 = E.throw $ DecodeError "The mailbox length is over 255"
mailbox o = validateMailbox $ Mailbox $ Domain{wireLabels_ = listWireLabels ls}
  where
    l = lowercase o
    ls = unfoldr step (l, 0 :: Int)
    step (x, n) = case parseLabel sep x of
        Nothing -> Nothing
        Just (p', x')
            | p' == "" -> Nothing
            | otherwise -> Just (p', (x', n + 1))
      where
        sep
            | n == 0 = _at
            | otherwise = _period

mailboxFromWireLabels :: WireLabels -> Mailbox
mailboxFromWireLabels lls
    | lls == emptyLabels = E.throw $ DecodeError "Broken mailbox"
    | otherwise = validateMailbox $ Mailbox $ Domain{wireLabels_ = lls}

instance IsRepresentation Mailbox ShortByteString where
    fromRepresentation = mailbox
    toRepresentation = toMailboxRep
    fromWireLabels = toMailbox . domainFromWireLabels . listWireLabels
    toWireLabels = wireLabels . fromMailbox

instance IsRepresentation Mailbox ByteString where
    fromRepresentation = mailbox . Short.toShort
    toRepresentation = Short.fromShort . toMailboxRep
    fromWireLabels = toMailbox . domainFromWireLabels . listWireLabels . (Short.toShort <$>)
    toWireLabels = (Short.fromShort <$>) . wireLabels . fromMailbox

instance IsRepresentation Mailbox String where
    fromRepresentation = mailbox . fromString
    toRepresentation = shortToString . toMailboxRep
    fromWireLabels = toMailbox . domainFromWireLabels . listWireLabels . (fromString <$>)
    toWireLabels = (shortToString <$>) . wireLabels . fromMailbox

mailboxSize :: Mailbox -> Int
mailboxSize = domainSize . fromMailbox

----------------------------------------------------------------

-- | Canonical-form flag.
-- For example, `Canonical` is used from DNSSEC extension.
--
-- ref. https://datatracker.ietf.org/doc/html/rfc4034#section-6.2 - Canonical RR Form
data CanonicalFlag
    = -- | Original name
      Original
    | -- | Lower name without compressoin
      Canonical
    deriving (Eq, Show)

----------------------------------------------------------------

-- | Putting a domain name.
--   No name compression for new RRs.
putDomain :: CanonicalFlag -> Domain -> Builder ()
putDomain Original Domain{..} wbuf _ = do
    mapM_ (putPartialDomain wbuf) wireLabels_
    put8 wbuf 0
putDomain Canonical Domain{..} wbuf _ = do
    mapM_ (putPartialDomain wbuf) wireLabels_ -- fixme
    put8 wbuf 0

putPartialDomain :: WriteBuffer -> Label -> IO ()
putPartialDomain wbuf dom = putLenShortByteString wbuf dom

----------------------------------------------------------------

putCompressedDomain :: Domain -> Builder ()
putCompressedDomain Domain{..} = putCompress wireLabels_

putCompress :: WireLabels -> Builder ()
putCompress dom wbuf ref = case uncons (Array.elems dom) of
    Nothing -> put8 wbuf 0
    Just (d, ds) -> do
        mpos <- popPointer dom ref
        cur <- position wbuf
        case mpos of
            Just pos -> putPointer wbuf pos
            _ -> do
                -- Pointers are limited to 14-bits!
                when (cur <= 0x3fff) $ pushPointer dom cur ref
                putPartialDomain wbuf d
                putCompress (listWireLabels ds) wbuf ref

putPointer :: WriteBuffer -> Int -> IO ()
putPointer wbuf pos = putInt16 wbuf (pos .|. 0xc000)

-- | Putting a domain name.
--   Names are compressed if possible.
--   This should be used only for CNAME, MX, NS, PTR and SOA.
putDomainRFC1035 :: CanonicalFlag -> Domain -> Builder ()
putDomainRFC1035 Original dom = putCompressedDomain dom
putDomainRFC1035 Canonical dom = putDomain Canonical dom

----------------------------------------------------------------

-- | Putting a mailbox.
--   No name compression for new RRs.
putMailbox :: CanonicalFlag -> Mailbox -> Builder ()
putMailbox cf (Mailbox d) = putDomain cf d

-- | Putting a mailbox.
--   Names are compressed if possible.
--   This should be used only for SOA.
putMailboxRFC1035 :: CanonicalFlag -> Mailbox -> Builder ()
putMailboxRFC1035 cf (Mailbox d) = putDomainRFC1035 cf d

----------------------------------------------------------------

-- | Getting a domain name.
--   An error is thrown if name compression is used.
getDomain :: Parser Domain
getDomain rbuf ref =
    domainFromWireLabels . listWireLabels <$> do
        n <- position rbuf
        getDomain' False n rbuf ref

-- | Getting a domain name.
-- Pointers MUST point back into the packet per RFC1035 Section 4.1.4.  This
-- is further interpreted by the DNS community (from a discussion on the IETF
-- DNSOP mailing list) to mean that they don't point back into the same domain.
-- Therefore, when starting to parse a domain, the current offset is also a
-- strict upper bound on the targets of any pointers that arise while processing
-- the domain.  When following a pointer, the target again becomes a stict upper
-- bound for any subsequent pointers.  This results in a simple loop-prevention
-- algorithm, each sequence of valid pointer values is necessarily strictly
-- decreasing!
getDomainRFC1035 :: Parser Domain
getDomainRFC1035 rbuf ref =
    domainFromWireLabels . listWireLabels <$> do
        n <- position rbuf
        getDomain' True n rbuf ref

-- | Getting a mailbox.
--   An error is thrown if name compression is used.
getMailbox :: Parser Mailbox
getMailbox rbuf ref =
    mailboxFromWireLabels . listWireLabels <$> do
        n <- position rbuf
        getDomain' False n rbuf ref

-- | Getting a mailbox.
getMailboxRFC1035 :: Parser Mailbox
getMailboxRFC1035 rbuf ref =
    mailboxFromWireLabels . listWireLabels <$> do
        n <- position rbuf
        getDomain' True n rbuf ref

-- $
--
-- The case below fails to point far enough back, and triggers the loop
-- prevention code-path.
--
-- >>> let parser = getDomain' True 0
-- >>> let input = "\3foo\192\0\3bar\0"
-- >>> runParser parser input
-- Left (DecodeError "invalid pointer 0 at 4: self pointing")

-- | Get a domain name.
--
-- Domain name compression pointers must always refer to a position
-- that precedes the start of the current domain name.  The starting
-- offsets form a strictly decreasing sequence, which prevents pointer
-- loops.
getDomain' :: Bool -> Int -> Parser Labels
getDomain' allowCompression ptrLimit = \rbuf ref -> do
    pos <- position rbuf
    c <- getInt8 rbuf
    let n = getValue c
    getdomain pos c n rbuf ref
  where
    getdomain pos c n rbuf ref
        | c == 0 = do
            pushDomain pos [] ref
            return []
        -- As for now, extended labels have no use.
        -- This may change some time in the future.
        | isExtLabel c = return []
        | isPointer c && not allowCompression =
            failParser "name compression is not allowed"
        | isPointer c = do
            d <- getInt8 rbuf
            let offset = n * 256 + d
            when (offset == ptrLimit) $ failure "self pointing" pos offset
            when (offset > ptrLimit) $ failure "forward pointing" pos offset
            mx <- popDomain offset ref
            case mx of
                Nothing -> failure "invalid area" pos offset
                Just lls -> do
                    -- Supporting double pointers.
                    pushDomain pos lls ref
                    return lls
        | otherwise = do
            l <- lowercase <$> getNShortByteString rbuf n
            -- Registering super domains
            ls <- getDomain' allowCompression ptrLimit rbuf ref
            let lls = l : ls
            pushDomain pos lls ref
            return lls
    -- The length label is limited to 63.
    getValue c = c .&. 0x3f
    isPointer c = testBit c 7 && testBit c 6
    isExtLabel c = not (testBit c 7) && testBit c 6
    failure msg pos offset =
        failParser $
            "invalid pointer " ++ show offset ++ " at " ++ show pos ++ ": " ++ msg

----------------------------------------------------------------

-- | Decode a domain name in A-label form to a leading label and a tail with
-- the remaining labels, unescaping backlashed chars and decimal triples along
-- the way. Any U-label conversion belongs at the layer above this code.
-- 'Nothing' means that the input domain illegal.
--
-- >>> parseLabel _period "abc.def.xyz"
-- Just ("abc","def.xyz")
-- >>> parseLabel _period "abc.def.xyz."
-- Just ("abc","def.xyz.")
-- >>> parseLabel _period "xyz."
-- Just ("xyz","")
-- >>> parseLabel _period "abc\\.def.xyz"
-- Just ("abc.def","xyz")
-- >>> parseLabel _period "\\097.xyz"
-- Just ("a","xyz")
-- >>> parseLabel _period ".abc.def.xyz"
-- Nothing
-- >>> parseLabel _period "\\09.xyz"
-- Nothing
-- >>> parseLabel _period "\\513.xyz"
-- Nothing
parseLabel
    :: Word8 -> Label -> Maybe (Label, Label)
parseLabel sep dom
    | hasBackslash dom = case P.parse (labelParser sep mempty) dom of
        (Just hd, tl) -> check (hd, tl)
        _ -> Nothing
    | otherwise = case Short.break (== sep) dom of
        r@(_, "") -> Just r
        (hd, tl) -> check (hd, Short.drop 1 tl)
  where
    hasBackslash = Short.any (== _backslash)
    check r@(hd, tl)
        | not (Short.null hd) || Short.null tl = Just r
        | otherwise = Nothing

labelParser :: Word8 -> P.Builder -> P.Parser P.Builder
labelParser sep bld =
    (P.eof $> bld)
        <|> (P.satisfy (== sep) $> bld)
        <|> (simple >>= \b -> labelParser sep (bld <> b))
        <|> (escaped >>= \b -> labelParser sep (bld <> b))
  where
    simple = P.toBuilder . fst <$> P.match skipUnescaped
      where
        skipUnescaped = P.skipSome $ P.satisfy notSepOrBslash
        notSepOrBslash w = w /= sep && w /= _backslash

    escaped = do
        P.skip (== _backslash)
        ddd <|> nonDDD
      where
        nonDDD = P.toBuilder <$> P.anyChar
        ddd = do
            x <- digit
            y <- digit <|> fail "an digit is expected (1)"
            z <- digit <|> fail "an digit is expected (2)"
            let d = 100 * x + 10 * y + z
            if d > 255
                then fail "DDD should be less than 256"
                else pure (P.toBuilder (fromIntegral d :: Word8))
        digit :: P.Parser Int -- Word8 is not good enough for "d > 255"
        digit = fromIntegral . subtract _0 <$> P.satisfy isDigit

----------------------------------------------------------------

-- | Convert a wire-form label to presentation-form by escaping
-- the separator, special and non-printing characters.  For simple
-- labels with no bytes that require escaping we get back the input
-- Text asis with no copying or re-construction.
--
-- Note: the separator is required to be either \'.\' or \'\@\', but this
-- constraint is the caller's responsibility and is not checked here.
--
-- >>> escapeLabel _period "foo"
-- "foo"
-- >>> escapeLabel _period "foo.bar"
-- "foo\\.bar"
-- >>> escapeLabel _period "\x0aoo"
-- "\\010oo"
-- >>> escapeLabel _period "f\x7fo"
-- "f\\127o"
escapeLabel :: Word8 -> Label -> Label
escapeLabel sep label
    | isAllPlain label = label
    | otherwise = toResult $ P.parse (labelEscaper sep mempty) label
  where
    isAllPlain = Short.all (isPlain sep)
    toResult (Just r, _) = r
    toResult _ = E.throw UnknownDNSError -- can't happen

labelEscaper :: Word8 -> P.Builder -> P.Parser P.Builder
labelEscaper sep bld0 =
    (P.eof $> bld0)
        <|> (asis >>= \b -> labelEscaper sep (bld0 <> b))
        <|> (esc >>= \b -> labelEscaper sep (bld0 <> b))
  where
    -- Non-printables are escaped as decimal trigraphs, while printable
    -- specials just get a backslash prefix.
    esc = do
        w <- P.anyChar
        if w <= _space || w >= _del
            then
                let (q100, r100) = w `divMod` 100
                    (q10, r10) = r100 `divMod` 10
                 in pure
                        ( P.toBuilder _backslash
                            <> P.toBuilder (_0 + q100)
                            <> P.toBuilder (_0 + q10)
                            <> P.toBuilder (_0 + r10)
                        )
            else pure (P.toBuilder _backslash <> P.toBuilder w)

    -- Runs of plain bytes are recognized as a single chunk, which is then
    -- returned as-is.
    asis :: P.Parser P.Builder
    asis = do
        (r, _) <- P.match $ P.skipSome $ P.satisfy (isPlain sep)
        return $ P.toBuilder r

-- | In the presentation form of DNS labels, these characters are escaped by
-- prepending a backlash. (They have special meaning in zone files). Whitespace
-- and other non-printable or non-ascii characters are encoded via "\DDD"
-- decimal escapes. The separator character is also quoted in each label. Note
-- that '@' is quoted even when not the separator.
escSpecials :: [Word8]
escSpecials = [_quotedbl, _dollar, _parenleft, _parenright, _semicolon, _at, _backslash]

-- | Is the given byte the separator or one of the specials?
isSpecial :: Word8 -> Word8 -> Bool
isSpecial sep w = w == sep || elem w escSpecials

-- | Is the given byte a plain byte that reqires no escaping. The tests are
-- ordered to succeed or fail quickly in the most common cases. The test
-- ranges assume the expected numeric values of the named special characters.
-- Note: the separator is assumed to be either '.' or '@' and so not matched by
-- any of the first three fast-path 'True' cases.
isPlain :: Word8 -> Word8 -> Bool
isPlain sep w
    | w >= _del = False -- <DEL> + non-ASCII
    | w >= _bracketright = True -- ']'..'_'..'a'..'z'..'~'
    | w >= _A && w <= _bracketleft = True -- 'A'..'Z'..'['
    | w >= _0 && w <= _colon = True -- '0'..'9'..':'
    | w <= _space = False -- non-printables
    | isSpecial sep w = False -- one of the specials
    | otherwise = True -- plain punctuation

----------------------------------------------------------------

shortToString :: ShortByteString -> String
shortToString = C8.unpack . Short.fromShort

----------------------------------------------------------------

-- | `superDomains' u d` super domains of `d` with upper bound `u`
--
-- >>> superDomains' "c." "a.b.c."
-- ["b.c.","a.b.c."]
-- >>> superDomains' "." "."
-- []
superDomains' :: Domain -> Domain -> [Domain]
superDomains' ul d0@(Domain wl0) = go (Array.elems wl0) [d0]
  where
    go ls ss = case uncons ls of
        Nothing -> [] -- only the case of rootDomain
        Just (_, ls')
            | wl' == ul' -> ss
            | otherwise -> go ls' (Domain wl' : ss)
          where
            wl' = listWireLabels ls'
    Domain ul' = ul

-- | Creating super domains.
--
-- >>> superDomains "www.example.com"
-- ["com.","example.com.","www.example.com."]
-- >>> superDomains "www.example.com."
-- ["com.","example.com.","www.example.com."]
-- >>> superDomains "com."
-- ["com."]
-- >>> superDomains "."
-- []
superDomains :: Domain -> [Domain]
superDomains = superDomains' "."

-- | Sub-domain or not.
--
-- >>> "www.example.com." `isSubDomainOf` "."
-- True
-- >>> "www.example.com." `isSubDomainOf` "com."
-- True
-- >>> "www.example.com." `isSubDomainOf` "example.com."
-- True
-- >>> "www.example.com." `isSubDomainOf` "www.example.com."
-- True
-- >>> "www.example.com." `isSubDomainOf` "foo-www.example.com."
-- False
isSubDomainOf :: Domain -> Domain -> Bool
_ `isSubDomainOf` "." = True
Domain dx `isSubDomainOf` Domain dy =
    dx == dy
        || let xs = Array.elems dx
               ys = Array.elems dy
            in ys `isSuffixOf` xs

-- | just count labels of domain
labelsCount :: Domain -> Int
labelsCount = Array.numElements . wireLabels_

----------------------------------------------------------------

lowercase :: ShortByteString -> ShortByteString
lowercase s
    | Short.any isUpper s = Short.map toLower s
    | otherwise = s

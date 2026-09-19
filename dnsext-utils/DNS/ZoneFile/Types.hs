module DNS.ZoneFile.Types where

-- ghc packages
import qualified Data.ByteString.Short as Short
import Data.Char (chr, ord)
import Data.List (unfoldr)
import Data.String (IsString (..))
import Data.Word (Word8)

-- dnsext-* packages
import DNS.Types

-- this package
import DNS.Parser

raise :: MonadParser t s m => String -> m a
raise = parseError

---

data Directive
    = D_Origin
    | D_TTL
    deriving (Eq, Show)

{- FOURMOLU_DISABLE -}
data Word8E
    = C Word8  -- ^ not escaped byte
    | E Word8  -- ^     escaped byte
    deriving (Eq, Show)
{- FOURMOLU_ENABLE -}

unEscW8 :: Word8E -> Word8
unEscW8 (C w8) = w8
unEscW8 (E w8) = w8

isEscaped :: Word8E -> Bool
isEscaped C{} = False
isEscaped E{} = True

-- character-string or longer opaque-string
type CString = Short.ShortByteString

cstringW8 :: [Word8] -> CString
cstringW8 = Short.pack

fromCString :: CString -> String
fromCString = map (chr . fromIntegral) . Short.unpack

-- character-string, character-string with escaped info, or longer opaque-string
type EString = [Word8E]

-- | A character-string: its bytes, and which of them the file wrote as
--   an escape.
--
--   Which bytes were escaped is asked about twice -- by 'show', and by
--   the SVCB parameter parser, where a @\\,@ is not a separator -- and
--   both are rare.  Keeping it as the 'EString' the lexer read cost a
--   cons cell and a boxed 'Word8E' for every byte of every
--   character-string in the file, which is most of a zone file: reading
--   sixty thousand records held twenty megabytes of them.  The bytes
--   are already in 'cs_cs', so all that is left to keep is a flag each,
--   and nothing at all for a string with no escape in it, which is
--   nearly every one.
data CS' = CS' {cs_cs :: CString, cs_escaped :: Maybe CString} deriving (Eq)

-- | The character-string as the lexer read it, byte by byte.
cs_es :: CS' -> EString
cs_es CS'{cs_cs = s, cs_escaped = mfs} = case mfs of
    Nothing -> [C w | w <- Short.unpack s]
    Just fs -> zipWith mark (Short.unpack fs) (Short.unpack s)
  where
    mark 0 w = C w
    mark _ w = E w

{- FOURMOLU_DISABLE -}
instance Show CS' where
    show cs@(CS'{cs_cs = s, cs_escaped = mfs})
        | Just _ <- mfs  = show $ cs_es cs
        | otherwise      = show s
{- FOURMOLU_ENABLE -}

instance IsString CS' where
    -- naive instance for tests
    fromString s = CS'{cs_cs = cstringW8 ws, cs_escaped = Nothing}
      where
        ws = [fromIntegral $ ord c | c <- s]

estringToCS' :: EString -> CS'
estringToCS' es = CS'{cs_cs = cstringW8 ws, cs_escaped = mfs}
  where
    ws = [unEscW8 e | e <- es]
    mfs
        | any isEscaped es = Just $ Short.pack [if isEscaped e then 1 else 0 | e <- es]
        | otherwise = Nothing

data Token
    = Directive Directive
    | At
    | LParen
    | RParen
    | Blank
    | Dot
    | CS CS'
    | Comment
    | RSep
    deriving (Eq, Show)

instance ParserToken Token

---

type Line = [Token]

-- $setup
-- >>> :seti -XOverloadedStrings

{- FOURMOLU_DISABLE -}
-- |
-- >>> reduceParens1 [[CS "a",LParen,CS "b"], [CS "c",RParen,CS "d",LParen,CS "e"], [CS "f",RParen,CS "g"], [CS "h"]]
-- Just ([CS "a",LParen,CS "b",CS "c",RParen,CS "d",LParen,CS "e",CS "f",RParen,CS "g"],[[CS "h"]])
reduceParens1 :: [Line] -> Maybe (Line, [Line])
reduceParens1 [] = Nothing
reduceParens1 (ts0:xs0) = Just $ scan id ts0 xs0
  where
    scan a  []         xs  = (a [], xs)
    scan a (LParen:ts) xs  = inner (a . (LParen :)) ts xs
    scan a (t:ts)      xs  = scan (a . (t :)) ts xs

    inner a  []          []     = (a [], [])  {- mismatch case. missing last RParen -}
    inner a  []         (x:xs)  = inner a x xs
    inner a (RParen:ts)  xs     = scan (a . (RParen :)) ts xs
    inner a (t:ts) xs           = inner (a . (t :)) ts xs
{- FOURMOLU_ENABLE -}

reduceParens :: [Line] -> [Line]
reduceParens = unfoldr reduceParens1

{- FOURMOLU_DISABLE -}
-- |
-- >>> normLine [CS "example",Dot,CS "com",Dot,Blank,CS "7200",Blank,LParen,CS "IN",Blank,CS "A",Blank,CS "203",Dot,CS "0",Dot,CS "113",Dot,CS "3",Blank,RParen,Blank,Comment]
-- [CS "example",Dot,CS "com",Dot,Blank,CS "7200",Blank,CS "IN",Blank,CS "A",Blank,CS "203",Dot,CS "0",Dot,CS "113",Dot,CS "3"]
-- >>> normLine [Blank,Comment]
-- []
normLine :: Line -> Line
normLine s0
    | null s1                 = []
    | last s1 `elem` asBlank  = init s1  {- drop last blank -}
    | otherwise               = s1
  where
    s1 = rec_ id s0

    rec_ a []               = a []
    rec_ a (t:ts)
        | t `elem` asBlank  = blank (a . (Blank :))  ts
        | otherwise         = rec_  (a . (t :))      ts

    blank a []              = rec_   a               []
    blank a (t:ts)
        | t `elem` asBlank  = blank  a               ts
        | otherwise         = rec_  (a . (t :))      ts
    {- reduce some blanks to one -}
    asBlank = [Blank, Comment, LParen, RParen]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | convert to record separated tokens
normTokens :: [Line] -> [Token]
normTokens = concatMap (rsep . normLine) . reduceParens
  where
    {- skip blank record -}
    rsep     []    = []
    rsep xs@(_:_)  = xs ++ [RSep]
{- FOURMOLU_ENABLE -}

---

data Record
    = R_Origin Domain
    | R_TTL TTL
    | R_RR ResourceRecord
    deriving (Show)

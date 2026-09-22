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

data CS' = CS' {cs_cs :: CString, cs_es :: EString} deriving (Eq)

{- FOURMOLU_DISABLE -}
instance Show CS' where
    show (CS'{cs_cs = s, cs_es = es})
        | any isEscaped es  = show es
        | otherwise         = show s
{- FOURMOLU_ENABLE -}

instance IsString CS' where
    -- naive instance for tests
    fromString s = CS'{cs_cs = cstringW8 ws, cs_es = [C w | w <- ws]}
      where
        ws = [fromIntegral $ ord c | c <- s]

estringToCS' :: EString -> CS'
estringToCS' es = CS'{cs_cs = cstringW8 [unEscW8 e | e <- es], cs_es = es}

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

{- FOURMOLU_DISABLE -}
-- | The lexer puts an 'RSep' at the end of each record, which is where
--   a line ends, so that is where the line number goes up.  Without
--   this the instance was empty and took the default, which counts
--   every token as a column and never leaves line one: an error in the
--   sixth line of a zone file was reported at "line 1, column 60",
--   which is a lie about the line and a lie about the column.
--
--   What is left of a character position by the time the lexer has
--   been through it is nothing, so the second number counts tokens
--   within the line and 'posUnit' says as much.
instance ParserToken Token where
    proceed RSep  (lin, _  )  = (lin + 1, 0)
    proceed _     (lin, col)  = (lin, col + 1)
    posUnit _                 = "token"
{- FOURMOLU_ENABLE -}

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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module DNS.Config (
    Conf,
    ConfValue (..),
    FromConf (..),
    loadFile,
    loadNested,
    loadInclude,
    readArg,
    nestedLimit,
) where

import Control.Applicative
import qualified DNS.Log as Log
import DNS.Parser hiding (Parser)
import DNS.Types (OD_NSID (..))
import qualified DNS.Types.Opaque as Opaque
import Data.Char (toUpper)
import Data.Functor
import Data.List
import Data.List.Split (splitOn)
import Data.String (fromString)
import Network.Socket (PortNumber)
import System.IO.Error (ioeSetErrorString, tryIOError)
import System.Posix (GroupID, UserID, getGroupEntryForName, getUserEntryForName, groupID, userID)

import DNS.Config.Parser

----------------------------------------------------------------

type Conf = (String, ConfValue)

data ConfValue = CV_Int Int | CV_Bool Bool | CV_String String | CV_Strings [String] deriving (Eq, Show)

class FromConf a where
    fromConf :: ConfValue -> IO a

instance FromConf Int where
    fromConf (CV_Int n) = pure n
    fromConf cv = failWith cv "fromConf int"

instance FromConf PortNumber where
    fromConf (CV_Int n) = pure $ fromIntegral n
    fromConf cv = failWith cv "fromConf port"

instance FromConf Bool where
    fromConf (CV_Bool b) = pure b
    fromConf cv = failWith cv "fromConf bool"

instance FromConf String where
    fromConf (CV_String s) = pure s
    fromConf (CV_Strings []) = pure ""
    fromConf (CV_Strings (s : _)) = pure s
    fromConf cv = failWith cv "fromConf string"

instance FromConf (Maybe String) where
    fromConf (CV_String "") = pure Nothing
    fromConf (CV_String s) = pure $ Just s
    fromConf cv = failWith cv "fromConf maybe string"

instance FromConf [String] where
    -- splitOn is for backward compatibly only so
    -- it should be removed in the future.
    --     fromConf (CV_String s) = pure [s]
    fromConf (CV_String s) = pure $ filter (/= "") $ splitOn "," s
    fromConf (CV_Strings ss) = pure ss
    fromConf cv = failWith cv "fromConf string list"

instance FromConf (Maybe OD_NSID) where
    fromConf (CV_String "") = pure Nothing
    fromConf (CV_String s) = Just <$> decodeNSID s
    fromConf cv = failWith cv "fromConf maybe NSID"

instance FromConf UserID where
    fromConf (CV_String s) = uidForName s
    fromConf (CV_Int i) = pure $ fromIntegral i
    fromConf cv = failWith cv "fromConf user-ID"

instance FromConf GroupID where
    fromConf (CV_String s) = gidForName s
    fromConf (CV_Int i) = pure $ fromIntegral i
    fromConf cv = failWith cv "fromConf group-ID"

instance FromConf Log.Level where
    fromConf (CV_String s) = logLevel s
    fromConf cv = failWith cv "fromConf log level"

failWith :: Show a => a -> String -> IO b
failWith x s = fail (s ++ ": " ++ show x)

decodeNSID :: String -> IO OD_NSID
decodeNSID s =
    maybe (fail "nsid: NSID must be hex-string or ascii-string with ascii_ prefix") (pure . OD_NSID) $ decodeAscii <|> decodeB16
  where
    decodeAscii = fromString <$> stripPrefix "ascii_" s
    decodeB16 = either (\_ -> Nothing) Just $ Opaque.fromBase16 $ fromString s

{- FOURMOLU_DISABLE -}
uidForName :: String -> IO UserID
uidForName s = either (nameError ("user: " ++ s)) (pure . userID) =<< tryIOError (getUserEntryForName s)

gidForName :: String -> IO GroupID
gidForName s = either (nameError ("group: " ++ s)) (pure . groupID) =<< tryIOError (getGroupEntryForName s)

nameError :: String -> IOError -> IO a
nameError n ioe = ioError $ ioeSetErrorString ioe n
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

logLevel :: String -> IO Log.Level
logLevel s = case lvs of
    lv : _ -> pure lv
    [] -> fail $ "fromConf unknwon log-level " ++ s
  where
    lvs = [lv | (lv, "") <- reads u]
    u = map toUpper s

----------------------------------------------------------------

loadFile :: FilePath -> IO [Conf]
loadFile = parseFile config

loadNested :: Int -> [Conf] -> IO [Conf]
loadNested = expandNested loadInclude

loadInclude :: FilePath -> IO [Conf]
loadInclude path = do
    putStrLn $ "loading included conf: " ++ path
    parseFile config path

{- FOURMOLU_DISABLE -}
-- |
-- >>> include = pure . maybe [] id . (`lookup` [("n1", [("include", CV_String "n3")]), ("n2", [("bar", CV_Int 2)]), ("n3", [("baz", CV_Int 5)])])
-- >>> runNested n cs = expandNested include n cs
-- >>> runNested 0 []
-- *** Exception: ... limit exceeded...
-- >>> runNested 5 []
-- []
-- >>> runNested 1 [("include", CV_String "n1")]
-- *** Exception: ... limit exceeded...
-- >>> runNested 5 [("foo", CV_Int 3)]
-- [("foo",CV_Int 3)]
-- >>> runNested 5 [("foo", CV_Int 3), ("include", CV_String "n2")]
-- [("foo",CV_Int 3),("bar",CV_Int 2)]
-- >>> runNested 5 [("foo", CV_Int 3), ("include", CV_String "n2"), ("bar", CV_Int 7), ("include", CV_String "n1"), ("baz", CV_Int 6)]
-- [("foo",CV_Int 3),("bar",CV_Int 2),("bar",CV_Int 7),("baz",CV_Int 5),("baz",CV_Int 6)]
expandNested
    :: (FilePath -> IO [Conf])
    -> Int -> [Conf] -> IO [Conf]
expandNested _       n _
    | n <= 0              = fail $ "config: config-file nested-inclusion limit is " ++ show nestedLimit ++ ", limit exceeded."
expandNested loadInc n cs0 = include cs0
  where
    include  []           = pure []
    include (c@(k, v):cs)
        | k == "include"  = fromConf v >>= \path -> (++) <$> (expandNested loadInc (n - 1) =<< loadInc path) <*> include cs
        | otherwise       = chunk (c:) cs
    chunk ac      []      = pure (ac [])
    chunk ac ccs@(c@(k, _):cs)
        | k == "include"  = (ac [] ++) <$> include ccs
        | otherwise       = chunk (ac . (c:)) cs

nestedLimit :: Int
nestedLimit = 5
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

readArg :: String -> IO Conf
readArg = parseString arg

----------------------------------------------------------------

config :: MonadParser W8 s m => m [Conf]
config = commentLines *> many cfield <* eof
  where
    cfield = field <* commentLines

-- |
-- >>> parse field "" "int: 3\n"
-- Right ("int",CV_Int 3)
-- >>> parse field "" "bool: yes\n"
-- Right ("bool",CV_Bool True)
-- >>> parse field "" "str: foo\n"
-- Right ("str",CV_String "foo")
-- >>> parse field "" "prefix-int: 127.0.0.1,::1 # comment \n"
-- Right ("prefix-int",CV_String "127.0.0.1,::1")
-- >>> parse field "" "prefix-bool-1: nothing # comment \n"
-- Right ("prefix-bool-1",CV_String "nothing")
-- >>> parse field "" "prefix-bool-2: yesterday # comment \n"
-- Right ("prefix-bool-2",CV_String "yesterday")
-- >>> parse field "" "d-quoted: \"foo bar baz\" # foo\n"
-- Right ("d-quoted",CV_String "foo bar baz")
-- >>> parse field "" "s-quoted: 'foo bar baz' # foo\n"
-- Right ("s-quoted",CV_String "foo bar baz")
-- >>> parse field "" "list: \"a b\" c\n"
-- Right ("list",CV_Strings ["a b","c"])
-- >>> parse field "" "listc: \"d e\" f # comment \n"
-- Right ("listc",CV_Strings ["d e","f"])
-- >>> parse field "" "  bool: yes\n"
-- Right ("bool",CV_Bool True)
field :: MonadParser W8 s m => m Conf
field = (,) <$> key <*> (sep *> value) <* trailing

arg :: MonadParser W8 s m => m Conf
arg = (,) <$> key <*> (char '=' *> value)

key :: MonadParser W8 s m => m String
key = spcs *> some (oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-") <* spcs

sep :: MonadParser W8 s m => m ()
sep = void $ char ':' *> spcs

squote :: MonadParser W8 s m => m ()
squote = void $ char '\''

dquote :: MonadParser W8 s m => m ()
dquote = void $ char '"'

value :: MonadParser W8 s m => m ConfValue
value = choice [cv_int, cv_bool, cv_strings]

eov :: MonadParser W8 s m => m ()
eov = void (lookAhead $ choice [char '#', char ' ', char '\n']) <|> eof

-- Trailing should be included in try to allow IP addresses.
cv_int :: MonadParser W8 s m => m ConfValue
cv_int = CV_Int . read <$> some digit <* eov

{- FOURMOLU_DISABLE -}
cv_bool :: MonadParser W8 s m => m ConfValue
cv_bool =
    CV_Bool True <$ string "yes" <* eov <|>
    CV_Bool False <$ string "no" <* eov

cv_string' :: MonadParser W8 s m => m String
cv_string' =
    squote *> many (noneOf "'\n")  <* squote <|>
    dquote *> many (noneOf "\"\n") <* dquote <|>
    some (noneOf "\"# \t\n")
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- |
-- >>> parse cv_strings "" ""
-- Right (CV_Strings [])
-- >>> parse cv_strings "" "\"conf.txt\""
-- Right (CV_String "conf.txt")
-- >>> parse cv_strings "" "\"example. 1800 TXT 'abc'\" static"
-- Right (CV_Strings ["example. 1800 TXT 'abc'","static"])
cv_strings :: MonadParser W8 s m => m ConfValue
cv_strings = strings <|> pure (CV_Strings [])
  where
    strings = do
        v1 <- cv_string'
        vs <- many (spcs1 *> cv_string')
        pure $
            if null vs
                then CV_String v1
                else CV_Strings $ v1 : vs
{- FOURMOLU_ENABLE -}

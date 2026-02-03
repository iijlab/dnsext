module DNS.ZoneFile (
    LexParser,
    ZoneParser,
    runLexParser,
    runZoneParser,
    module DNS.ZoneFile.Types,
    module DNS.ZoneFile.Lexer,
    module DNS.ZoneFile.Parser,
    module DNS.ZoneFile.IO,
) where

-- ghc packages
import qualified Data.ByteString.Lazy as LB

-- this package
import DNS.Parser (runParser)
import DNS.ZoneFile.IO
import DNS.ZoneFile.Lexer hiding (Parser)
import qualified DNS.ZoneFile.Lexer as L
import DNS.ZoneFile.Parser hiding (Parser, parseFile, parseFile', parseLineRR, runParser)
import qualified DNS.ZoneFile.Parser as P
import DNS.ZoneFile.Types

type LexParser a = L.Parser a

runLexParser :: LexParser a -> LB.ByteString -> Either String (a, LB.ByteString)
runLexParser = runParser

type ZoneParser a = P.Parser a

runZoneParser :: ZoneParser a -> Context -> [Token] -> Either String ((a, Context), [Token])
runZoneParser = P.runParser

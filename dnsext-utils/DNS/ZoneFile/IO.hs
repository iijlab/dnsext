module DNS.ZoneFile.IO where

-- ghc packages
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO.Unsafe (unsafeInterleaveIO)

-- dnsext-* packages
import DNS.Types (Domain, ResourceRecord)

-- this package
import DNS.ZoneFile.Lexer (lexLine, lexLineAt)
import DNS.ZoneFile.Parser (Context)
import qualified DNS.ZoneFile.Parser as P
import DNS.ZoneFile.Types as T

parseLineRR :: L8.ByteString -> Context -> Either String (ResourceRecord, Context)
parseLineRR s cxt = do
    ts <- lexLine s
    P.parseLineRR (T.normLine ts) cxt

parseLine :: L8.ByteString -> Context -> Either String (Record, Context)
parseLine s cxt = do
    ts <- lexLine s
    P.parseLineRecord (T.normLine ts) cxt

-- | Every line of a zone file lexed, and the whole of it parsed.  The lexing is
--   lazy IO: a line which will not lex fails where the parser reaches it.
parseFile :: FilePath -> Domain -> IO [Record]
parseFile fn dom = do
    bslines <- L8.lines <$> LB.readFile fn
    tklines <- lazily $ zipWith (\lin -> either fail pure . lexLineAt lin) [1 ..] bslines
    either fail (pure . fst) $ P.parseFile dom $ T.normTokens tklines
  where
    lazily = foldr (\io ios -> unsafeInterleaveIO ((:) <$> io <*> ios)) (pure [])

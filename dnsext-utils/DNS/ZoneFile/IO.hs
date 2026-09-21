module DNS.ZoneFile.IO where

-- ghc packages
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8

-- dnsext-* packages
import DNS.Types (Domain, ResourceRecord)

-- this package
import DNS.ZoneFile.Lexer (lexLine)
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

-- | Every line of a zone file lexed, and the whole of it parsed.
--
--   The lexing is lazy: a line's tokens are built as the parser reaches
--   them and are rubbish as soon as it has gone past.  Lexing the file
--   first, as a pass of its own, held every token of it at once --
--   several hundred megabytes for a zone of a hundred and eighty
--   thousand records, none of it wanted by the end -- and that is what
--   decided how much memory reading a zone took.
--
--   A line which will not lex throws where it is looked at.  That is
--   inside the parse below and so inside this action, and what it
--   throws is the 'userError' 'fail' threw before, so the caller is
--   told the same thing in the same way.  What differs is a file with
--   both a parse error and, after it, a lexing error: the first of the
--   two is now reported rather than the second.
parseFile :: FilePath -> Domain -> IO [Record]
parseFile fn dom = do
    bslines <- L8.lines <$> LB.readFile fn
    let tklines = map (either (E.throw . userError) id . lexLine) bslines
    either fail (pure . fst) $ P.parseFile dom $ T.normTokens tklines

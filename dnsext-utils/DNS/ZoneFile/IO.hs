module DNS.ZoneFile.IO where

-- ghc packages
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

parseFile :: FilePath -> IO [Record]
parseFile fn = do
    bslines <- L8.lines <$> LB.readFile fn
    tklines <- either fail pure $ mapM lexLine bslines
    either fail (pure . fst) $ P.parseFile $ T.normTokens tklines

parseFile' :: FilePath -> Domain -> IO [Record]
parseFile' fn dom = do
    bslines <- L8.lines <$> LB.readFile fn
    tklines <- either fail pure $ mapM lexLine bslines
    either fail (pure . fst) $ P.parseFile' dom $ T.normTokens tklines

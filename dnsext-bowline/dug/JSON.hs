{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module JSON (showJSON) where

import DNS.Types
import DNS.Types.Encode
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Prettyprinter
import Prettyprinter.Render.String

showJSON :: DNSMessage -> String
showJSON msg@DNSMessage{..} = toString $ object (hd ++ q ++ [an, au, ad])
  where
    DNSFlags{..} = flags
    hd =
        [ ("ID", pretty identifier)
        , ("QR", pretty $ fromBool isResponse)
        , ("Opcode", pretty $ fromOPCODE opcode)
        , ("comment", string opcode)
        , ("AA", pretty $ fromBool authAnswer)
        , ("TC", pretty $ fromBool trunCation)
        , ("RD", pretty $ fromBool recDesired)
        , ("RA", pretty $ fromBool recAvailable)
        , ("AD", pretty $ fromBool authenData)
        , ("CD", pretty $ fromBool chkDisable)
        , ("RCode", pretty $ fromRCODE rcode)
        , ("comment", string rcode)
        , ("QDCOUNT", pretty (1 :: Int))
        , ("ANCOUNT", pretty $ length answer)
        , ("NSCOUNT", pretty $ length authority)
        , ("ARCOUNT", pretty $ arCountEDNS msg)
        ]
    Question{..} = question
    q =
        [ ("QNAME", domain qname)
        , ("QTYPE", pretty $ fromTYPE qtype)
        , ("QTYPEname", string qtype)
        , ("QCLASS", pretty $ fromCLASS qclass)
        ]
    an = rrs "answerRRs" answer
    au = rrs "authorityRRs" authority
    ad = rrs "additionalRRs" additional

toString :: Doc ann -> String
toString = renderString . layoutPretty defaultLayoutOptions

object :: [(String, Doc ann)] -> Doc ann
object [] = align (lbrace <> rbrace)
object kvs = align (vsep ([lbrace] ++ punctuate comma pairs ++ [rbrace]))
  where
    pairs = map pair kvs

array :: [Doc ann] -> Doc ann
array [] = align (lbracket <> rbracket)
array objs = align (vsep ([lbracket] ++ punctuate comma objs ++ [rbracket]))

rrs :: String -> [ResourceRecord] -> (String, Doc ann)
rrs k xs = (k, array $ map rr xs)

rr :: ResourceRecord -> Doc ann
rr ResourceRecord{..} = object xs
  where
    Seconds ttl = rrttl
    xs =
        [ ("NAME", domain rrname)
        , ("TYPE", pretty $ fromTYPE rrtype)
        , ("TYPEname", string rrtype)
        , ("TTL", pretty ttl)
        , rd rrtype rdata
        ]

{- FOURMOLU_DISABLE -}
rd :: TYPE -> RData -> (String, Doc ann)
rd A     v = ("rdataA",     justQuote (fromRData v :: Maybe RD_A))
rd AAAA  v = ("rdataAAAA",  justQuote (fromRData v :: Maybe RD_AAAA))
rd CNAME v = ("rdataCNAME", just (fromRData v :: Maybe RD_CNAME))
rd DNAME v = ("rdataDNAME", just (fromRData v :: Maybe RD_DNAME))
rd NS    v = ("rdataNS",    just (fromRData v :: Maybe RD_NS))
rd PTR   v = ("rdataPTR",   just (fromRData v :: Maybe RD_PTR))
rd TXT   v = ("rdataTXT",   just (fromRData v :: Maybe RD_TXT))
rd _     v = ("RDATAHEX",   dquotes $ pretty $ B.unpack $ B16.encode $ encodeRData v)
{- FOURMOLU_ENABLE -}

domain :: Domain -> Doc ann
domain = dquotes . pretty . B.unpack . toRepresentation

string :: Show a => a -> Doc ann
string = dquotes . pretty . show

justQuote :: Show a => Maybe a -> Doc ann
justQuote = dquotes . pretty . show . fromJust

just :: Show a => Maybe a -> Doc ann
just = pretty . show . fromJust

pair :: (String, Doc ann) -> Doc ann
pair (k, v) = dquotes (pretty k) <> colon <+> v

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

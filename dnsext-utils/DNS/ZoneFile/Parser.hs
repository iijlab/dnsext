{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStrict #-}

module DNS.ZoneFile.Parser where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Functor

-- dnsext-* packages
import DNS.SEC
import DNS.Types hiding (rrclass, rrname, rrttl, rrtype)
import qualified DNS.Types.Opaque as Opaque
import Data.IP (IPv4, IPv6)

-- this package
import DNS.Parser hiding (Parser, runParser)
import qualified DNS.Parser as Poly
import DNS.ZoneFile.ParserBase
import DNS.ZoneFile.ParserDNSSEC
import DNS.ZoneFile.ParserSVCB
import DNS.ZoneFile.Types

{- FOURMOLU_DISABLE -}
data Context =
    Context
    { cx_zone   :: Domain
    , cx_name   :: Domain
    , cx_ttl    :: TTL
    , cx_class  :: CLASS
    }

instance Show Context where
    show (Context z n t c) = unwords ["Context", show z, show n, show $ toInteger t, show c]

defaultContext :: Context
defaultContext =
    Context
    { cx_zone   = "."
    , cx_name   = "."
    , cx_ttl    = 1800
    , cx_class  = IN
    }
{- FOURMOLU_ENABLE -}

type Parser = StateT Context (Poly.Parser [Token])

runParser :: Parser a -> Context -> [Token] -> Either String ((a, Context), [Token])
runParser p = Poly.runParser . runStateT p

instance MonadParser Token [Token] Parser where
    getInput = lift getInput
    putInput = lift . putInput
    raiseParser = lift . raiseParser
    getPos = lift getPos
    putPos = lift . putPos

setCx :: (a -> Context -> Context) -> a -> Parser a
setCx set_ x = modify (set_ x) $> x

setZone :: Domain -> Parser Domain
setZone = setCx (\x s -> s{cx_zone = x})

setName :: Domain -> Parser Domain
setName = setCx (\x s -> s{cx_name = x})

setTTL :: TTL -> Parser TTL
setTTL = setCx (\x s -> s{cx_ttl = x})

setClass :: CLASS -> Parser CLASS
setClass = setCx (\x s -> s{cx_class = x})

---

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import DNS.SEC (addResourceDataForDNSSEC)
-- >>> import DNS.SVCB (addResourceDataForSVCB)
-- >>> runInitIO addResourceDataForDNSSEC
-- >>> runInitIO addResourceDataForSVCB
-- >>> cx = Context "" "" 3600 IN

type Labels = [CString]

-- | not empty relative domain labels
rlabels' :: MonadParser Token s m => m (Labels -> Labels)
rlabels' = (++) <$> ((:) <$> cstring <*> many (dot *> cstring))

-- | absolute domain labels
alabels :: MonadParser Token s m => m Labels
alabels = (rlabels' <|> pure id {- root case -}) <*> (dot $> [])

rlabels :: Parser Labels
rlabels = rlabels' <*> (toLabels <$> gets cx_zone)

toLabels :: IsRepresentation a CString => a -> Labels
toLabels = toWireLabels

fromLabels :: IsRepresentation a CString => Labels -> a
fromLabels = fromWireLabels

-- | absolute domain name
-- >>> runParser adomain cx [CS "example",Dot,CS "net",Dot]
-- Right (("example.net.",Context "." "." 3600 IN),[])
adomain :: MonadParser Token s m => m Domain
adomain = fromLabels <$> alabels

-- | not empty relative domain name
-- >>> runParser rdomain cx{cx_zone = "net."} [CS "example"]
-- Right (("example.net.",Context "net." "." 3600 IN),[])
rdomain :: Parser Domain
rdomain = fromLabels <$> rlabels

{- FOURMOLU_DISABLE -}
-- |
-- >>> runParser domain cx [CS "b",Dot,CS "example",Dot,CS "net",Dot]
-- Right (("b.example.net.",Context "." "." 3600 IN),[])
-- >>> runParser domain cx{cx_zone = "example.net."} [CS "a"]
-- Right (("a.example.net.",Context "example.net." "." 3600 IN),[])
-- >>> runParser domain cx{cx_zone = "example.net."} [At]
-- Right (("example.net.",Context "example.net." "." 3600 IN),[])
domain :: Parser Domain
domain =
    adomain                         <|>
    rdomain                         <|>
    this At *> gets cx_zone

mailbox :: Parser Mailbox
mailbox = fromLabels <$> ( alabels  <|> rlabels )
{- FOURMOLU_ENABLE -}

-- |
-- >>> runParser seconds cx [CS "7200"]
-- Right ((7200(2 hours),Context "." "." 3600 IN),[])
seconds :: Parser Seconds
seconds = Seconds <$> readCString "seconds"

ttl :: Parser TTL
ttl = seconds

-- |
-- >>> runParser class_ cx [CS "IN"]
-- Right ((IN,Context "." "." 3600 IN),[])
class_ :: MonadParser Token s m => m CLASS
class_ = this (CS "IN") $> IN

-- |
-- >>> runParser type_ cx [CS "AAAA"]
-- Right ((AAAA,Context "." "." 3600 IN),[])
type_ :: MonadParser Token s m => m TYPE
type_ = readCString "type"

---

-- |
-- >>> runParser ipv4 cx [CS "203",Dot,CS "0",Dot,CS "113",Dot,CS "3"]
-- Right ((203.0.113.3,Context "." "." 3600 IN),[])
ipv4 :: MonadParser Token s m => m IPv4
ipv4 = join $ readv4 <$> cstrstr <*> (dot *> cstrstr) <*> (dot *> cstrstr) <*> (dot *> cstrstr)
  where
    cstrstr = fromCString <$> cstring
    readv4 a b c d = readable "Zonefile.ipv4" $ a ++ "." ++ b ++ "." ++ c ++ "." ++ d

-- |
-- >>> runParser ipv6 cx [CS "2001:db8::3"]
-- Right ((2001:db8::3,Context "." "." 3600 IN),[])
ipv6 :: MonadParser Token s m => m IPv6
ipv6 = readCString "ipv6"

---

rdataA :: MonadParser Token s m => m RData
rdataA = rd_a <$> ipv4

rdataAAAA :: MonadParser Token s m => m RData
rdataAAAA = rd_aaaa <$> ipv6

rdataPTR :: Parser RData
rdataPTR = rd_ptr <$> domain

rdataTXT :: MonadParser Token s m => m RData
rdataTXT = rd_txt_n . txts <$> ((:) <$> nbstring <*> many (blank *> nbstring))
  where
    txts = map Opaque.fromShortByteString
    nbstring = mconcat <$> some (cstring <|> dot $> ".")

rdataMX :: Parser RData
rdataMX = rd_mx <$> readCString "mx_preference" <*> (blank *> domain)

rdataNS :: Parser RData
rdataNS = rd_ns <$> domain

rdataCNAME :: Parser RData
rdataCNAME = rd_cname <$> domain

{- FOURMOLU_DISABLE -}
rdataSOA :: Parser RData
rdataSOA =
    rd_soa
    <$> domain <*> (blank *> mailbox) <*> (blank *> readCString "soa_serial")
    <*> (blank *> seconds) <*> (blank *> seconds) <*> (blank *> seconds) <*> (blank *> seconds)
{- FOURMOLU_ENABLE -}

---

{- FOURMOLU_DISABLE -}
{-- $ORIGIN <domain-name> [<comment>]
      --(normalized)-->
      $ORIGIN <blank> <domain-name>  -}
-- |
-- >>> runParser zoneOrigin cx [Directive D_Origin,Blank,CS "example",Dot,CS "net",Dot]
-- Right (("example.net.",Context "example.net." "." 3600 IN),[])
zoneOrigin :: Parser Domain
zoneOrigin = this (Directive D_Origin) *> this Blank *> adomain >>= setZone
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
{-- $TTL <ttl> [<comment>]
      --(normalized)-->
      $TTL <blank> <ttl>  -}
-- |
-- >>> runParser zoneTTL cx [Directive D_TTL,Blank,CS "7200"]
-- Right ((7200(2 hours),Context "." "." 7200 IN),[])
zoneTTL :: Parser TTL
zoneTTL = this (Directive D_TTL) *> this Blank *> ttl >>= setTTL
{- FOURMOLU_ENABLE -}

{- NOT SUPPORT --- $INCLUDE <file-name> [<domain-name>] [<comment>] -}

{-- <blank><rr> [<comment>]
    <domain-name><rr> [<comment>]
      --(normalized)-->
      (<domain-name>) <blank> <TTL> <blank> <class> <blank> <type> <blank> <RDATA>
      (<domain-name>) <blank> <class> <blank> <TTL> <blank> <type> <blank> <RDATA>
      (<domain-name>) <blank> <TTL> <blank> <type> <blank> <RDATA>
      (<domain-name>) <blank> <class> <blank> <type> <blank> <RDATA>               -}

rrname :: Parser Domain
rrname = optional domain >>= maybe (gets cx_name) setName

rrttl :: Parser TTL
rrttl = optional (blank *> ttl) >>= maybe (gets cx_ttl) setTTL

rrclass :: Parser CLASS
rrclass = optional (blank *> class_) >>= maybe (gets cx_class) setClass

{- FOURMOLU_DISABLE -}
-- |
-- >>> -- example from RFC9460 - Appendix D. Test Vectors - D.2. ServiceMode - Fiture 10
-- >>> -- * URL: https://datatracker.ietf.org/doc/html/rfc9460#name-servicemode-3
-- >>> -- * example RR: example.com.   SVCB   16 foo.example.org. alpn="f\\\\oo\\,bar,h2"
-- >>> runParser (rrTyRData (,)) cx [Blank,CS "SVCB",Blank,CS "16",Blank,CS "foo",Dot,CS "example",Dot,CS "org",Dot,Blank,CS "alpn=",CS (estringToCS' [C 102,E 92,C 111,C 111,E 44,C 98,C 97,C 114,C 44,C 104,C 50])]
-- Right (((SVCB,16 "foo.example.org." alpn=["f\\oo,bar","h2"]),Context "." "." 3600 IN),[])
rrTyRData :: (TYPE -> RData -> a) -> Parser a
rrTyRData mk =
    blank *>
    ( type_ >>=
      pair
      ([ (A      , rdataA      )
       , (AAAA   , rdataAAAA   )
       , (PTR    , rdataPTR    )
       , (TXT    , rdataTXT    )
       , (NS     , rdataNS     )
       , (MX     , rdataMX     )
       , (CNAME  , rdataCNAME  )
       , (SOA    , rdataSOA    )
       ] ++
       rdatasDNSSEC ++
       rdatasSVCB domain)
    )
  where
    pair tbl ty = do
         let left = raise $ "Zonefile.rdata: unsupported TYPE: " ++ show ty
             right rd = mk ty <$> (blank *> rd :: Parser RData {- for GHC 9.2. type-inference not working with fundep? -})
         maybe left right (lookup ty tbl)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- |
-- >>> runParser zoneRR cx [CS "example",Dot,CS "net",Dot,Blank,CS "7200",Blank,CS "IN",Blank,CS "AAAA",Blank,CS "2001:db8::3"]
-- Right ((ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3},Context "." "example.net." 7200 IN),[])
-- >>> runParser zoneRR cx [CS "example",Dot,CS "net",Dot,Blank,CS "IN",Blank,CS "7200",Blank,CS "AAAA",Blank,CS "2001:db8::3"]
-- Right ((ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3},Context "." "example.net." 7200 IN),[])
-- >>> runParser zoneRR cx{cx_name = "example.net."} [Blank,CS "7200",Blank,CS "IN",Blank,CS "AAAA",Blank,CS "2001:db8::3"]
-- Right ((ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3},Context "." "example.net." 7200 IN),[])
-- >>> runParser zoneRR cx{cx_name = "example.net.", cx_ttl = 7200, cx_class = IN} [Blank,CS "AAAA",Blank,CS "2001:db8::3"]
-- Right ((ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3},Context "." "example.net." 7200 IN),[])
zoneRR :: Parser ResourceRecord
zoneRR =
    (pat1 <$> rrname <*> rrttl <*> rrclass >>= rrTyRData)  <|>
    (pat2 <$> rrname <*> rrclass <*> rrttl >>= rrTyRData)
 where
   pat1 = \name ttl_ cls typ rd -> ResourceRecord name typ cls ttl_ rd
   pat2 = \name cls ttl_ typ rd -> ResourceRecord name typ cls ttl_ rd
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
record :: Parser Record
record =
    R_RR      <$> zoneRR      <|>
    R_Origin  <$> zoneOrigin  <|>
    R_TTL     <$> zoneTTL
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
file :: Parser [Record]
file = many (record <* this RSep)
{- FOURMOLU_ENABLE -}

-- |
-- >>> parseLineRR [CS "example",Dot,CS "net",Dot,Blank,CS "7200",Blank,CS "IN",Blank,CS "AAAA",Blank,CS "2001:db8::3"] defaultContext
-- Right (ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3},Context "." "example.net." 7200 IN)
-- >>> parseLineRR [Dot,Blank,CS "IN",Blank,CS "DS",Blank,CS "20326",Blank,CS "8",Blank,CS "2",Blank,CS "E06D44B80B8F1D39A95C0B0D7C65D08458E880409BBC683457104237C7F8EC8D"] defaultContext
-- Right (ResourceRecord {rrname = ".", rrtype = DS, rrclass = IN, rrttl = 1800(30 mins), rdata = RD_DS {ds_key_tag = 20326, ds_pubalg = RSASHA256, ds_digestalg = SHA256, ds_digest = \# 32 e06d44b80b8f1d39a95c0b0d7c65d08458e880409bbc683457104237c7f8ec8d}},Context "." "." 1800 IN)
-- >>> parseLineRR [CS "_dns",Dot,CS "resolver",Dot,CS "arpa",Dot,Blank,CS "300",Blank,CS "IN",Blank,CS "SVCB",Blank,CS "1",Blank,CS "ns",Dot,CS "example",Dot,Blank,CS "alpn=",CS "h2,h3",Blank,CS "ipv4hint=192.0.2.19",Blank,CS "ipv6hint=2001:db8::13"] defaultContext
-- Right (ResourceRecord {rrname = "_dns.resolver.arpa.", rrtype = SVCB, rrclass = IN, rrttl = 300(5 mins), rdata = 1 "ns.example." alpn=["h2","h3"] ipv4hint=[192.0.2.19] ipv6hint=[2001:db8::13]},Context "." "_dns.resolver.arpa." 300 IN)
parseLineRR :: [Token] -> Context -> Either String (ResourceRecord, Context)
parseLineRR ts icontext = fst <$> runParser (zoneRR <* eof) icontext ts

-- |
-- >>> parseLineRecord [Directive D_Origin,Blank,CS "example",Dot,CS "net",Dot] defaultContext
-- Right (R_Origin "example.net.",Context "example.net." "." 1800 IN)
-- >>> parseLineRecord [CS "example",Dot,CS "net",Dot,Blank,CS "7200",Blank,CS "IN",Blank,CS "AAAA",Blank,CS "2001:db8::3"] defaultContext
-- Right (R_RR (ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3}),Context "." "example.net." 7200 IN)
parseLineRecord :: [Token] -> Context -> Either String (Record, Context)
parseLineRecord ts icontext = fst <$> runParser (record <* eof) icontext ts

-- |
-- >>> parseFile [CS "example",Dot,CS "net",Dot,Blank,CS "7200",Blank,CS "IN",Blank,CS "AAAA",Blank,CS "2001:db8::3",RSep]
-- Right ([R_RR (ResourceRecord {rrname = "example.net.", rrtype = AAAA, rrclass = IN, rrttl = 7200(2 hours), rdata = 2001:db8::3})],Context "." "example.net." 7200 IN)
parseFile :: [Token] -> Either String ([Record], Context)
parseFile = (fst <$>) . runParser (file <* eof) defaultContext

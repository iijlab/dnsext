{-# LANGUAGE RecordWildCards #-}

module Output (pprResult, OutputFlag (..)) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.Maybe (catMaybes)
import Data.Monoid (Endo (..))

import qualified DNS.SEC.Verify as Verify
import DNS.Types

----------------------------------------------------------------

data OutputFlag
    = Singleline
    | Multiline
    | JSONstyle
    | Short
    deriving (Eq, Show)

type Flags = [OutputFlag]
type Print = ReaderT Flags (Writer (Endo String))
type Printer a = a -> Print ()

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
pprResult :: [OutputFlag] -> DNSMessage -> String
pprResult [Short] msg = runPrinter resultShort []    msg
pprResult flags   msg = runPrinter result      flags msg
{- FOURMOLU_ENABLE -}

runPrinter :: Printer a -> Flags -> a -> String
runPrinter p oflags x = execWriter (runReaderT (p x) oflags) `appEndo` ""

getFlags :: Print Flags
getFlags = ask

----------------------------------------------------------------

resultShort :: Printer DNSMessage
resultShort DNSMessage{..} = rrsShort answer

rrsShort :: Printer [ResourceRecord]
rrsShort [] = return ()
rrsShort (r0 : rs0) = do
    rrShort r0
    loop rs0
  where
    loop [] = return ()
    loop (r : rs) = do
        nl
        rrShort r
        loop rs

rrShort :: Printer ResourceRecord
rrShort ResourceRecord{..} = do
    let prettyRData _ = show rdata
    string . prettyRData =<< getFlags
    let keyTag dnskey = string (" (key_tag: " ++ show (Verify.keyTag dnskey) ++ ")")
    maybe (pure ()) keyTag $ fromRData rdata

----------------------------------------------------------------

result :: Printer DNSMessage
result DNSMessage{..} = do
    putHeader (identifier, opcode, rcode, flags)
    nl
    putEDNSHeader ednsHeader
    putQS question
    putRRS answers answer
    putRRS authoritys authority
    putRRS additionals additional

----------------------------------------------------------------

putHeader :: Printer (Identifier, OPCODE, RCODE, DNSFlags)
putHeader (idnt, op, rc, flags) = do
    dsemi *> sp *> string "HEADER SECTION:"
    nl
    semi *> string (opcd op) *> string ", "
    string (show rc) *> string ", "
    string "id: " *> string (show idnt)
    nl
    semi *> string "Flags:" *> sp *> putFlags flags
    nl

putFlags :: Printer DNSFlags
putFlags DNSFlags{..} = sepBy string (string ", ") $ catMaybes xs
  where
    jst x str = if x then Just str else Nothing
    xs :: [Maybe String]
    xs =
        [ jst authAnswer "Authoritative Answer"
        , jst trunCation "Truncated Caution"
        , jst recDesired "Recursion Desired"
        , jst recAvailable "Recursion Available"
        , jst authenData "Authentic Data"
        , jst chkDisable "Checking Disabled"
        ]

{- FOURMOLU_DISABLE -}
opcd :: OPCODE -> String
opcd OP_STD    = "Standard query"
opcd OP_INV    = "Inverse query"
opcd OP_SSR    = "Server status request"
opcd OP_NOTIFY = "Change notification"
opcd OP_UPDATE = "Update request"
opcd x = show x
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

putEDNSHeader :: Printer EDNSheader
putEDNSHeader (EDNSheader EDNS{..}) = do
    nl
    dsemi *> sp *> string "OPTIONAL PSEUDO EDNS SECTION:" *> nl
    semi
    string "Version: " *> string (show ednsVersion) *> string ", "
    string "UDP: " *> string (show ednsUdpSize) *> string ", "
    string "DNSSEC OK: " *> string (show ednsDnssecOk) *> string ", " {- DNSFlags have other flags -}
    string "Data:" *> string (show ednsOptions) *> nl
putEDNSHeader _ = pure ()

----------------------------------------------------------------

putQS :: [Question] -> Print ()
putQS qs = do
    nl
    dsemi *> sp *> string "QUESTION SECTION:" *> nl
    mapM_ qq qs

qq :: Printer Question
qq Question{..} = do
    semi *> string (toRepresentation qname)
    tab
    tab
    string $ cls qclass
    tab
    string $ show qtype
    nl

----------------------------------------------------------------

putRRS :: Printer [ResourceRecord] -> [ResourceRecord] -> Print ()
putRRS ppr rs = nl *> ppr rs

answers :: Printer Answers
answers = rrs "ANSWER SECTION:"

authoritys :: Printer AuthorityRecords
authoritys = rrs "AUTHORITY SECTION:"

additionals :: Printer AdditionalRecords
additionals = rrs "ADDITIONAL SECTION:"

rrs :: String -> Printer [ResourceRecord]
rrs name rs = do
    dsemi *> sp *> string name *> nl
    mapM_ rr rs

{- FOURMOLU_DISABLE -}
rr :: Printer ResourceRecord
rr ResourceRecord{..} = do
    string $ toRepresentation rrname
    tab
    string $ show rrttl
    tab
    string $ cls rrclass
    tab
    string $ show rrtype
    tab
    let prettyRData oflags
            | Multiline `elem` oflags  = prettyShowRData rdata
            | otherwise                = show rdata
    string . prettyRData =<< getFlags
    let keyTag dnskey = string (" (key_tag: " ++ show (Verify.keyTag dnskey) ++ ")")
    maybe (pure ()) keyTag $ fromRData rdata
    nl
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

cls :: CLASS -> String
cls c
    | c == IN = "IN"
    | otherwise = "#<" ++ show c ++ ">"

----------------------------------------------------------------

char :: Printer Char
char = lift . tell . Endo . (:)

string :: Printer String
string = mapM_ char

sepBy :: Printer a -> Print () -> Printer [a]
sepBy _ _ [] = pure ()
sepBy p s (x : xs) = p x *> mapM_ ((s *>) . p) xs

semi :: Print ()
semi = char ';'

dsemi :: Print ()
dsemi = semi *> semi

sp :: Print ()
sp = char ' '

tab :: Print ()
tab = char '\t'

nl :: Print ()
nl = char '\n'

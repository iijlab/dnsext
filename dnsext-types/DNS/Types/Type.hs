{-# LANGUAGE PatternSynonyms #-}

module DNS.Types.Type (
    TYPE (
        TYPE,
        A,
        NS,
        CNAME,
        SOA,
        NULL,
        PTR,
        MX,
        TXT,
        RP,
        AAAA,
        SRV,
        DNAME,
        OPT,
        TLSA,
        CSYNC,
        IXFR,
        AXFR,
        ANY,
        CAA
    ),
    fromTYPE,
    toTYPE,
    getTYPE,
    putTYPE,
    addType,
    allTYPEs,
) where

import Data.Char (toUpper)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map (Map)
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Text.Read

import DNS.Types.Imports
import DNS.Wire

----------------------------------------------------------------

-- | Types for resource records.
newtype TYPE = TYPE
    { fromTYPE :: Word16
    -- ^ From type to number.
    }
    deriving (Eq, Ord)

-- | From number to type.
toTYPE :: Word16 -> TYPE
toTYPE = TYPE

----------------------------------------------------------------

-- https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-4

-- | IPv4 address
pattern A :: TYPE
pattern A = TYPE 1

-- | An authoritative name serve
pattern NS :: TYPE
pattern NS = TYPE 2

-- | The canonical name for an alias
pattern CNAME :: TYPE
pattern CNAME = TYPE 5

-- | Marks the start of a zone of authority
pattern SOA :: TYPE
pattern SOA = TYPE 6

-- | A null RR (EXPERIMENTAL)
pattern NULL :: TYPE
pattern NULL = TYPE 10

-- | A domain name pointer
pattern PTR :: TYPE
pattern PTR = TYPE 12

-- | Mail exchange
pattern MX :: TYPE
pattern MX = TYPE 15

-- | Text strings
pattern TXT :: TYPE
pattern TXT = TYPE 16

-- | Responsible Person
pattern RP :: TYPE
pattern RP = TYPE 17

-- | IPv6 Address
pattern AAAA :: TYPE
pattern AAAA = TYPE 28

-- | Server Selection (RFC2782)
pattern SRV :: TYPE
pattern SRV = TYPE 33

-- | DNAME (RFC6672)
pattern DNAME :: TYPE
pattern DNAME = TYPE 39 -- RFC 6672

-- | OPT (RFC6891)
pattern OPT :: TYPE
pattern OPT = TYPE 41 -- RFC 6891

-- | Delegation Signer (RFC4034)
-- | TLSA (RFC6698)
pattern TLSA :: TYPE
pattern TLSA = TYPE 52 -- RFC 6698

-- | Child-To-Parent Synchronization (RFC7477)
pattern CSYNC :: TYPE
pattern CSYNC = TYPE 62 -- RFC 7477

-- | Incremental zone transfer (RFC1995)
pattern IXFR :: TYPE
pattern IXFR = TYPE 251 -- RFC 1995

-- | Zone transfer (RFC5936)
pattern AXFR :: TYPE
pattern AXFR = TYPE 252 -- RFC 5936

-- | A request for all records the server/cache has available
pattern ANY :: TYPE
pattern ANY = TYPE 255

-- | Certification Authority Authorization (RFC6844)
pattern CAA :: TYPE
pattern CAA = TYPE 257 -- RFC 6844

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
typeAndNames :: [(TYPE, String)]
typeAndNames =
    [ (A,     "A")
    , (NS,    "NS")
    , (CNAME, "CNAME")
    , (SOA,   "SOA")
    , (NULL,  "NULL")
    , (PTR,   "PTR")
    , (MX,    "MX")
    , (TXT,   "TXT")
    , (RP,    "RP")
    , (AAAA,  "AAAA")
    , (SRV,   "SRV")
    , (DNAME, "DNAME")
    , (OPT,   "OPT")
    , (TLSA,  "TLSA")
    , (CSYNC, "CSYNC")
    , (IXFR,  "IXFR")
    , (AXFR,  "AXFR")
    , (ANY,   "ANY")
    , (CAA,   "CAA")
    ]
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

instance Show TYPE where
    show (TYPE w) = case IM.lookup i dict of
        Nothing -> "TYPE" ++ show w
        Just name -> name
      where
        i = fromIntegral w
        dict = unsafePerformIO $ readIORef globalTypeShowDict

type TypeShowDict = IntMap String

insertTypeShowDict :: TYPE -> String -> TypeShowDict -> TypeShowDict
insertTypeShowDict (TYPE w) name dict = IM.insert i name dict
  where
    i = fromIntegral w

defaultTypeShowDict :: TypeShowDict
defaultTypeShowDict = foldr (uncurry insertTypeShowDict) IM.empty typeAndNames

{-# NOINLINE globalTypeShowDict #-}
globalTypeShowDict :: IORef TypeShowDict
globalTypeShowDict = unsafePerformIO $ newIORef defaultTypeShowDict

-- | All available types.
allTYPEs :: IO [String]
allTYPEs = map snd . IM.toList <$> readIORef globalTypeShowDict

----------------------------------------------------------------

instance Read TYPE where
    readListPrec = readListPrecDefault
    readPrec = do
        ms <- lexP
        let str0 = case ms of
                Ident s -> s
                String s -> s
                _ -> fail "Read TYPE"
            str = map toUpper str0
            dict = unsafePerformIO $ readIORef globalTypeReadDict
        case M.lookup str dict of
            Just t -> return t
            Nothing
                | "TYPE" `isPrefixOf` str -> return $ toTYPE $ read $ drop 4 str
                | otherwise -> fail "Read TYPE"

type TypeReadDict = Map String TYPE

insertTypeReadDict :: TYPE -> String -> TypeReadDict -> TypeReadDict
insertTypeReadDict t name dict = M.insert name t dict

defaultTypeReadDict :: TypeReadDict
defaultTypeReadDict = foldr (uncurry insertTypeReadDict) M.empty typeAndNames

{-# NOINLINE globalTypeReadDict #-}
globalTypeReadDict :: IORef TypeReadDict
globalTypeReadDict = unsafePerformIO $ newIORef defaultTypeReadDict

----------------------------------------------------------------

addType :: TYPE -> String -> IO ()
addType typ name = do
    atomicModifyIORef' globalTypeShowDict insShow
    atomicModifyIORef' globalTypeReadDict insRead
  where
    insShow dict = (insertTypeShowDict typ name dict, ())
    insRead dict = (insertTypeReadDict typ name dict, ())

----------------------------------------------------------------

getTYPE :: Parser TYPE
getTYPE rbuf _ = toTYPE <$> get16 rbuf

putTYPE :: TYPE -> Builder ()
putTYPE x wbuf _ = put16 wbuf $ fromTYPE x

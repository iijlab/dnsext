{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.DB (
    RRSetSig (..),
    IDB (..),
    allRRsofIDB,
    ODB (..),
    DB (..),
    dbRD_SOA,
    dbSOArr,
    getRRs,
    loadDB,
    makeDBforPrimary,
    makeDBforSecondary,
    emptyDB,
    loadZoneFile,
    lookupT,
    lookupD,
    NSECDB,
    DomainRange (..),
) where

import qualified Data.ByteString.Short as Short
import Data.Function (on)
import Data.List (groupBy, nub, partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, isNothing)
import qualified Data.Set as Set
import GHC.Stack

import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.ZoneFile as ZF

----------------------------------------------------------------

data IDB = IDB
    { idbAll :: [RRSetSig]
    , idbMap :: M.Map TYPE RRSetSig
    }
    deriving (Show)

emptyIDB :: IDB
emptyIDB = IDB{idbAll = [], idbMap = M.empty}

allRRsofIDB :: Bool -> IDB -> [ResourceRecord]
allRRsofIDB dnssecOK IDB{..} = concat $ map (getRRs dnssecOK) idbAll

getRRs :: Bool -> RRSetSig -> [ResourceRecord]
getRRs True RRSetSig{..} = rrsetsigRRs ++ maybe [] (: []) rrsetsigSig
getRRs False RRSetSig{..} = rrsetsigRRs

lookupT :: TYPE -> IDB -> Maybe RRSetSig
lookupT typ IDB{..} = M.lookup typ idbMap

data ODB = ODB
    { odbMap :: M.Map Domain IDB
    }
    deriving (Show)

lookupD :: Domain -> ODB -> Maybe IDB
lookupD dom ODB{..} = M.lookup dom odbMap

emptyODB :: ODB
emptyODB = ODB{odbMap = M.empty}

----------------------------------------------------------------

data DB = DB
    { dbZone :: Domain
    , dbSOA :: (RD_SOA, RRSetSig)
    , dbAnswer :: ODB
    , dbAuthority :: ODB
    , dbAdditional :: ODB
    , dbAll :: [ResourceRecord]
    , dbNsecMap :: NSECDB
    }
    deriving (Show)

dbRD_SOA :: DB -> RD_SOA
dbRD_SOA db = soa
  where
    (soa, _) = dbSOA db

dbSOArr :: Bool -> DB -> [ResourceRecord]
dbSOArr wantRRSig db = getRRs wantRRSig soarr
  where
    (_, soarr) = dbSOA db

----------------------------------------------------------------

emptyDB :: DB
emptyDB =
    DB
        { dbZone = "."
        , dbSOA = (soa, soarrsetsig)
        , dbAnswer = emptyODB
        , dbAuthority = emptyODB
        , dbAdditional = emptyODB
        , dbAll = []
        , dbNsecMap = M.empty
        }
  where
    soard = rd_soa "." "." 0 0 0 0 0
    soa = fromJust $ fromRData soard
    soarr =
        ResourceRecord
            { rrname = "."
            , rrtype = SOA
            , rrclass = IN
            , rrttl = 0
            , rdata = soard
            }
    soarrsetsig =
        RRSetSig
            { rrsetsigName = "."
            , rrsetsigType = SOA
            , rrsetsigRRs = [soarr]
            , rrsetsigSig = Nothing
            }

----------------------------------------------------------------

loadDB :: Domain -> FilePath -> IO (Maybe DB)
loadDB zone file = loadZoneFile zone file >>= makeDBforSecondary zone

loadZoneFile :: Domain -> FilePath -> IO [ResourceRecord]
loadZoneFile zone file = catMaybes . map fromResource <$> ZF.parseFile file zone

----------------------------------------------------------------

makeDBforPrimary
    :: Domain
    -> (Bool -> [ResourceRecord] -> IO [RRSetSig])
    -> [ResourceRecord]
    -> IO (Maybe DB)
makeDBforPrimary _ _ [] = return Nothing
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
makeDBforPrimary zone doSign (soarr : rrs)
    | rrtype soarr /= SOA = return Nothing
    | otherwise = case fromRData $ rdata soarr of
        Nothing -> return Nothing
        Just soa -> do
            let (ns, _os, gs, is) = divide zone rrs
            ssSigned <- doSign True [soarr]
            isSigned <- doSign True is
            nsSigned <- doSign True ns
            nsecSigned <- makeNSEC doSign $ ssSigned ++ isSigned ++ nsSigned
            let allrr =
                    getRRs True (unsafeHead ssSigned)
                        ++ concat (map (getRRs True) isSigned)
                        ++ concat (map (getRRs True) nsSigned)
                        ++ concat (map (getRRs True) nsecSigned)
                        ++ gs
                        ++ _os
                        ++ [soarr] -- for AXFR
            return $ Just $ makeDBFinal zone soa is gs ssSigned isSigned nsSigned nsecSigned allrr

makeDBforSecondary :: Domain -> [ResourceRecord] -> IO (Maybe DB)
makeDBforSecondary _ [] = return Nothing
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
makeDBforSecondary zone (soarr : rrs0)
    | rrtype soarr /= SOA = return Nothing
    | otherwise = case fromRData $ rdata soarr of
        Nothing -> return Nothing
        Just soa -> do
            let (sigs, rrs1) = partition (\r -> rrtype r == RRSIG) rrs0
                (nsec, rrs) = partition (\r -> rrtype r == NSEC) rrs1
            let (ns, _os, gs, is) = divide zone rrs
                sigDB = M.fromList $ catMaybes $ map rrsigKV sigs
                ssSigned = groupAndSig sigDB [soarr]
                isSigned = groupAndSig sigDB is
                nsSigned = groupAndSig sigDB ns
                nsecSigned = findSig sigDB nsec
            let allrr = [soarr] ++ rrs ++ [soarr] -- for AXFR
            return $ Just $ makeDBFinal zone soa is gs ssSigned isSigned nsSigned nsecSigned allrr

----------------------------------------------------------------

makeDBFinal
    :: Domain
    -> RD_SOA
    -> [ResourceRecord]
    -> [ResourceRecord]
    -> [RRSetSig]
    -> [RRSetSig]
    -> [RRSetSig]
    -> [RRSetSig]
    -> [ResourceRecord]
    -> DB
makeDBFinal zone soa is gs ssSigned isSigned nsSigned nsecSigned allrr =
    DB
        { dbZone = zone
        , dbSOA = (soa, unsafeHead ssSigned)
        , dbAnswer = ans
        , dbAuthority = auth
        , dbAdditional = add
        , dbAll = allrr
        , dbNsecMap = makeNSECDB nsecSigned
        }
  where
    ans = setEmptyNonTerminals zone $ makeODB (ssSigned ++ isSigned ++ nsecSigned)
    auth = setEmptyNonTerminals zone $ makeODB nsSigned
    as = filter (\r -> rrtype r == A || rrtype r == AAAA) is
    add = makeODB $ unsign (as ++ gs)

----------------------------------------------------------------

rrsigKV :: ResourceRecord -> Maybe ((Domain, TYPE), ResourceRecord)
rrsigKV rr = case fromRData $ rdata rr of
    Nothing -> Nothing
    Just rrsig -> Just ((rrname rr, rrsig_type rrsig), rr)

findSig
    :: M.Map (Domain, TYPE) ResourceRecord
    -> [ResourceRecord]
    -> [RRSetSig]
findSig db rrs0 = map (bindSIG db) $ map (: []) rrs0

groupAndSig
    :: M.Map (Domain, TYPE) ResourceRecord
    -> [ResourceRecord]
    -> [RRSetSig]
groupAndSig db rrs0 = map (bindSIG db) $ groupRRset rrs0

bindSIG :: M.Map (Domain, TYPE) ResourceRecord -> [ResourceRecord] -> RRSetSig
bindSIG db rrs =
    RRSetSig
        { rrsetsigName = rrname
        , rrsetsigType = rrtype
        , rrsetsigRRs = rrs
        , rrsetsigSig = msig
        }
  where
    ResourceRecord{..} = unsafeHead rrs
    msig = M.lookup (rrname, rrtype) db

unsign :: [ResourceRecord] -> [RRSetSig]
unsign rrs0 = map addNothing $ groupRRset rrs0
  where
    addNothing rrs =
        RRSetSig
            { rrsetsigName = rrname
            , rrsetsigType = rrtype
            , rrsetsigRRs = rrs
            , rrsetsigSig = Nothing
            }
      where
        ResourceRecord{..} = unsafeHead rrs

----------------------------------------------------------------

-- RFC 9471
-- In-domain and sibling glues only.
-- Unrelated glues are ignored.
-- ns: NS except this domain
-- _os: unrelated, ignored
-- gs: glue (in delegated domain)
-- is: in-domain
divide
    :: Domain
    -> [ResourceRecord]
    -> ([ResourceRecord], [ResourceRecord], [ResourceRecord], [ResourceRecord])
divide zone rrs = (ns, _os, gs, is)
  where
    -- ps: possible in-domain
    (ps, ns, _os) = partition3 zone rrs
    isDelegated = makeIsDelegated ns
    (gs, is) = partition (\r -> isDelegated (rrname r)) ps

partition3
    :: Domain
    -> [ResourceRecord]
    -> ( [ResourceRecord] -- Possible in-domain
       , [ResourceRecord] -- NS except this domain
       , [ResourceRecord] -- Unrelated, ignored
       )
partition3 dom rrs0 = loop rrs0 [] [] []
  where
    loop [] as ns os = (as, ns, os)
    loop (r : rs) as ns os
        | rrname r `isSubDomainOf` dom =
            if rrtype r == NS && rrname r /= dom
                then loop rs as (r : ns) os
                else loop rs (r : as) ns os
        | otherwise = loop rs as ns (r : os)

makeIsDelegated
    :: [ResourceRecord]
    -- ^ NS resource records
    -> (Domain -> Bool)
makeIsDelegated rrs = \dom -> or (map (\f -> f dom) ps)
  where
    s = Set.fromList $ map rrname rrs
    ps = map (\x -> (`isSubDomainOf` x)) $ Set.toList s

----------------------------------------------------------------

makeODB :: [RRSetSig] -> ODB
makeODB rs = ODB{odbMap = M.fromList kvs}
  where
    rss :: [[RRSetSig]]
    rss = groupBy ((==) `on` rrsetsigName) $ sort rs
    doms :: [Domain]
    doms = map (rrsetsigName . unsafeHead) rss
    kvs = zip doms $ map makeIDB rss

makeIDB :: [RRSetSig] -> IDB
makeIDB vs =
    IDB
        { idbAll = vs
        , idbMap = M.fromList ((RRSIG, rrsetsigRRSIG) : kvs)
        }
  where
    ks = map rrsetsigType vs
    kvs = zip ks vs
    rrsigs = catMaybes $ map rrsetsigSig vs
    rrsetsigRRSIG =
        RRSetSig
            { rrsetsigName = rrsetsigName $ unsafeHead vs
            , rrsetsigType = RRSIG
            , rrsetsigRRs = rrsigs
            , rrsetsigSig = Nothing
            }

unsafeHead :: HasCallStack => [a] -> a
unsafeHead (x : _) = x
unsafeHead _ = error "unsafeHead"

----------------------------------------------------------------

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

-- For RFC 4592 Sec 2.2.2.Empty Non-terminals
--
-- Example: _sip._tcp.example.com
-- _tcp.example.com exists but does not have RRs
setEmptyNonTerminals :: Domain -> ODB -> ODB
setEmptyNonTerminals zone (ODB m) = ODB m'
  where
    n = labelsCount zone
    doms0 = filter (\d -> labelsCount d >= n + 2) $ M.keys m
    doms1 = map snd $ catMaybes $ map unconsDomain doms0
    doms2 = concat $ map (drop n) $ map superDomains doms1
    doms3 = nub $ sort doms2
    ments = map (\d -> (d, M.lookup d m)) doms3
    ents = map fst $ filter (isNothing . snd) ments
    m' = foldr (\d db -> M.insert d emptyIDB db) m ents

----------------------------------------------------------------

data DomainRange = Exact Domain | Range Domain Domain deriving (Show)

{- FOURMOLU_DISABLE -}
instance Eq DomainRange where
    Exact k1      == Exact k2      = k1 == k2
    Range r1s r1e == Range r2s r2e = r1s == r2s && r1e == r2e
    Exact k       == Range rs re   = rs <= k    && k < re
    Range rs re   == Exact k       = rs <= k    && k < re

instance Ord DomainRange where
    Exact k1    <= Exact k2    = k1 <= k2
    Range _ r1e <= Range r2s _ = r1e <= r2s
    Exact k     <= Range rs _  = k <= rs
    Range _ re  <= Exact k     = re <= k
{- FOURMOLU_ENABLE -}

type NSECDB = M.Map DomainRange RRSetSig

makeNSEC
    :: (Bool -> [ResourceRecord] -> IO [RRSetSig])
    -> [RRSetSig]
    -> IO [RRSetSig]
makeNSEC doSign signed = doSign False $ map pack zipped
  where
    nameTypes :: [(Domain, TYPE)]
    nameTypes = map (\x -> (rrsetsigName x, rrsetsigType x)) signed
    packedNameTypes :: [(Domain, [TYPE])]
    packedNameTypes =
        map (\xs -> (fst (unsafeHead xs), map snd xs)) $
            groupBy ((==) `on` fst) nameTypes
    h = unsafeHead packedNameTypes
    slided = drop 1 packedNameTypes ++ [h]
    zipped :: [((Domain, [TYPE]), (Domain, [TYPE]))]
    zipped = zip packedNameTypes slided
    pack ((dom, types), (nxt, _)) =
        ResourceRecord
            { rrname = dom
            , rrclass = IN
            , rrtype = NSEC
            , rrttl = 3600
            , rdata = rd_nsec nxt (sort (NSEC : RRSIG : types))
            }

makeNSECDB :: [RRSetSig] -> NSECDB
makeNSECDB vals = M.fromList $ zip keys vals
  where
    keys = modifyTail $ catMaybes $ map unpack vals
    unpack :: RRSetSig -> Maybe (Domain, Domain)
    unpack rss = case fromRData $ rdata r of
        Nothing -> Nothing
        Just nsec -> Just (rrsetsigName rss, nsec_next_domain nsec)
      where
        r = unsafeHead $ rrsetsigRRs rss
    modifyTail [] = []
    modifyTail [(x, y)] = [Range x (modify y)]
    modifyTail ((x, y) : xys) = Range x y : modifyTail xys

    zero :: Short.ShortByteString
    zero = "\x00"
    modify :: Domain -> Domain
    modify dom = case toWireLabels dom of
        [] -> fromWireLabels [zero]
        l : ls -> fromWireLabels (l <> zero : ls)

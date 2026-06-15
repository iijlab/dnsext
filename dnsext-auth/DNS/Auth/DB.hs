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
    makeDB,
    makeDBforDNSSEC,
    emptyDB,
    loadZoneFile,
    lookupT,
    lookupD,
) where

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
loadDB zone file = makeDB zone <$> loadZoneFile zone file

loadZoneFile :: Domain -> FilePath -> IO [ResourceRecord]
loadZoneFile zone file = catMaybes . map fromResource <$> ZF.parseFile file zone

----------------------------------------------------------------

makeDB :: Domain -> [ResourceRecord] -> Maybe DB
makeDB _ [] = Nothing
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
makeDB zone (soarr : rrs)
    | rrtype soarr /= SOA = Nothing
    | otherwise = case fromRData $ rdata soarr of
        Nothing -> Nothing
        Just soa ->
            Just $
                DB
                    { dbZone = zone
                    , dbSOA = (soa, unsafeHead uu)
                    , dbAnswer = ans
                    , dbAuthority = auth
                    , dbAdditional = add
                    , dbAll = [soarr] ++ rrs ++ [soarr] -- for AXFR
                    }
  where
    -- RFC 9471
    -- In-domain and sibling glues only.
    -- Unrelated glues are ignored.
    -- as: possible in-domain
    -- ns: NS except this domain
    -- _os: unrelated, ignored
    (as, ns, _os) = partition3 zone rrs
    isDelegated = makeIsDelegated ns
    (gs, zs) = partition (\r -> isDelegated (rrname r)) as
    -- gs: glue (in delegated domain)
    -- zs: in-domain
    uu = unsign [soarr]
    ss = unsign zs
    ans = setEmptyNonTerminals zone $ makeODB (uu ++ ss)
    tt = unsign ns
    auth = setEmptyNonTerminals zone $ makeODB tt
    xs = filter (\r -> rrtype r == A || rrtype r == AAAA) zs
    add = makeODB $ unsign $ xs ++ gs

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

makeDBforDNSSEC
    :: Domain
    -> ([ResourceRecord] -> IO [RRSetSig])
    -> [ResourceRecord]
    -> IO (Maybe DB)
makeDBforDNSSEC _ _ [] = return Nothing
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
makeDBforDNSSEC zone doSign (soarr : rrs)
    | rrtype soarr /= SOA = return Nothing
    | otherwise = case fromRData $ rdata soarr of
        Nothing -> return Nothing
        Just soa -> do
            -- RFC 9471
            -- In-domain and sibling glues only.
            -- Unrelated glues are ignored.
            let (ps, ns, _os) = partition3 zone rrs
                -- ps: possible in-domain
                -- ns: NS except this domain
                -- _os: unrelated, ignored
                isDelegated = makeIsDelegated ns
                (gs, is) = partition (\r -> isDelegated (rrname r)) ps
            -- gs: glue (in delegated domain)
            -- is: in-domain
            ssSigned <- doSign [soarr]
            isSigned <- doSign is
            let ans = setEmptyNonTerminals zone $ makeODB (ssSigned ++ isSigned)
            nsSigned <- doSign ns
            let auth = setEmptyNonTerminals zone $ makeODB nsSigned
            let as = filter (\r -> rrtype r == A || rrtype r == AAAA) is
                add = makeODB $ unsign (as ++ gs)
                allrr =
                    getRRs True (unsafeHead ssSigned)
                        ++ concat (map (getRRs True) isSigned)
                        ++ concat (map (getRRs True) nsSigned)
                        ++ gs
                        ++ _os
                        ++ [soarr] -- for AXFR
            return $
                Just $
                    DB
                        { dbZone = zone
                        , dbSOA = (soa, unsafeHead ssSigned)
                        , dbAnswer = ans
                        , dbAuthority = auth
                        , dbAdditional = add
                        , dbAll = allrr
                        }

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

makeODB :: [RRSetSig] -> ODB
makeODB rs = ODB{odbMap = M.fromList kvs}
  where
    rss :: [[RRSetSig]]
    rss = groupBy ((==) `on` rrsetsigName) rs
    doms :: [Domain]
    doms = map (rrsetsigName . unsafeHead) rss
    kvs = zip doms $ map makeIDB rss

makeIDB :: [RRSetSig] -> IDB
makeIDB vs =
    IDB
        { idbAll = vs
        , idbMap = M.fromList kvs
        }
  where
    ks = map rrsetsigType vs
    kvs = zip ks vs

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

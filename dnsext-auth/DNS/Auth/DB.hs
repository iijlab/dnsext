{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.DB (
    RRSetSig (..),
    DB (..),
    dbRD_SOA,
    dbSOArr,
    getRRs,
    loadDB,
    makeDBforPrimary,
    makeDBforSecondary,
    emptyDB,
    loadZoneFile,
    NSECDB,
    lookupN,
    DomainRange (..),
    toWildcard,
    Result (..),
    lookupDB,
    CNAMECheck (..),
    checkCNAME,
) where

import qualified Data.ByteString.Short as Short
import Data.Function (on)
import Data.List (groupBy, nub, partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import GHC.Stack

import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.ZoneFile as ZF

----------------------------------------------------------------

toWildcard :: Domain -> Domain
toWildcard dom = case unconsDomain dom of
    Nothing -> dom
    Just (_, super) -> fromWireLabels ("*" : wireLabels super)

synthesize :: Domain -> RRSetSig -> RRSetSig
synthesize dom rs
    | isWildcard (rrsetsigName rs) =
        rs
            { rrsetsigName = dom
            , rrsetsigRRs = map syn $ rrsetsigRRs rs
            , rrsetsigSig = syn <$> rrsetsigSig rs
            }
    | otherwise = rs
  where
    syn r = r{rrname = dom}

isWildcard :: Domain -> Bool
isWildcard dom = case leafDomain dom of
    Nothing -> False
    Just l -> l == "*"

----------------------------------------------------------------

data DB = DB
    { dbZone :: Domain
    , dbLabelsCount :: Int
    , dbSOA :: (RD_SOA, RRSetSig)
    , dbNode :: Node
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

getRRs :: Bool -> RRSetSig -> [ResourceRecord]
getRRs True RRSetSig{..} = rrsetsigRRs ++ maybe [] (: []) rrsetsigSig
getRRs False RRSetSig{..} = rrsetsigRRs

----------------------------------------------------------------

emptyDB :: DB
emptyDB =
    DB
        { dbZone = "."
        , dbLabelsCount = 0
        , dbSOA = (soa, soarrsetsig)
        , dbNode = emptyNode
        , dbAll = []
        , dbNsecMap = emptyNSECDB
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
loadDB zone file = do
    rss <- loadZoneFile zone file
    makeDBforSecondary zone $ filter (\r -> rrtype r /= DS) rss

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
            let (is, ns, ds, gs, _os) = divide zone rrs
            ssSigned <- doSign True [soarr]
            isSigned <- doSign True is
            dsSigned <- doSign True ds
            -- In-domain NS/DS should have NSEC.
            nsecSigned <- makeNSECforPrimary doSign (soarr : (is ++ ns))
            let allrr =
                    getRRs True (unsafeHead ssSigned)
                        ++ concat (map (getRRs True) isSigned)
                        ++ ns
                        ++ concat (map (getRRs True) dsSigned)
                        ++ concat (map (getRRs True) nsecSigned)
                        ++ gs
                        ++ _os
                        ++ [soarr] -- for AXFR
            return $ Just $ makeDBFinal zone soa ns gs ssSigned isSigned dsSigned nsecSigned allrr

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
            let (is, ns, ds, gs, _os) = divide zone rrs
                sigDB = M.fromList $ catMaybes $ map rrsigKV sigs
                ssSigned = groupAndSig sigDB [soarr]
                isSigned = groupAndSig sigDB is
                dsSigned = groupAndSig sigDB ds
            let nsecSigned = makeNSECforSecondary sigDB nsec
            let allrr = [soarr] ++ rrs ++ [soarr] -- for AXFR
            return $ Just $ makeDBFinal zone soa ns gs ssSigned isSigned dsSigned nsecSigned allrr

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
makeDBFinal zone soa ns gs ssSigned isSigned dsSigned nsecSigned allrr =
    DB
        { dbZone = zone
        , dbLabelsCount = n
        , dbSOA = (soa, unsafeHead ssSigned)
        , dbNode = node
        , dbAll = allrr
        , dbNsecMap = makeNSECDB nsecSigned
        }
  where
    n = labelsCount zone
    node = makeNode n (ssSigned ++ isSigned ++ unsign ns ++ dsSigned ++ unsign gs)

----------------------------------------------------------------

rrsigKV :: ResourceRecord -> Maybe ((Domain, TYPE), ResourceRecord)
rrsigKV rr = case fromRData $ rdata rr of
    Nothing -> Nothing
    Just rrsig -> Just ((rrname rr, rrsig_type rrsig), rr)

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
-- is: in-domain
-- ns: NS except this domain
-- ds: DS
-- gs: glue (in delegated domain)
-- _os: unrelated, ignored
divide
    :: Domain
    -> [ResourceRecord]
    -> ([ResourceRecord], [ResourceRecord], [ResourceRecord], [ResourceRecord], [ResourceRecord])
divide zone rrs = (is, ns, ds, gs, _os)
  where
    -- ps: possible in-domain
    (ps, ns, ds, _os) = divide4 zone rrs
    isDelegated = makeIsDelegated ns
    (gs, is) = partition (\r -> isDelegated (rrname r)) ps

divide4
    :: Domain
    -> [ResourceRecord]
    -> ( [ResourceRecord] -- Possible in-domain
       , [ResourceRecord] -- NS except this domain
       , [ResourceRecord] -- DS
       , [ResourceRecord] -- Unrelated, ignored
       )
divide4 dom rrs0 = loop rrs0 [] [] [] []
  where
    loop [] as ns ds os = (as, ns, ds, os)
    loop (r : rs) as ns ds os
        | rrname r `isSubDomainOf` dom =
            if rrtype r == NS && rrname r /= dom
                then loop rs as (r : ns) ds os
                else
                    if rrtype r == DS
                        then loop rs as ns (r : ds) os
                        else loop rs (r : as) ns ds os
        | otherwise = loop rs as ns ds (r : os)

makeIsDelegated
    :: [ResourceRecord]
    -- ^ NS resource records
    -> (Domain -> Bool)
makeIsDelegated rrs = \dom -> or (map (\f -> f dom) ps)
  where
    s = Set.fromList $ map rrname rrs
    ps = map (\x -> (`isSubDomainOf` x)) $ Set.toList s

----------------------------------------------------------------

unsafeHead :: HasCallStack => [a] -> a
unsafeHead (x : _) = x
unsafeHead _ = error "unsafeHead"

----------------------------------------------------------------

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

----------------------------------------------------------------

makeNSECforPrimary
    :: (Bool -> [ResourceRecord] -> IO [RRSetSig])
    -> [ResourceRecord]
    -> IO [RRSetSig]
makeNSECforPrimary doSign rrs = doSign False $ map pack zipped
  where
    nameTypes :: [(Domain, TYPE)]
    nameTypes = map (\x -> (rrname x, rrtype x)) $ nub $ sort rrs
    packedNameTypes :: [(Domain, [TYPE])]
    packedNameTypes =
        map (\xs -> (fst (unsafeHead xs), nub $ sort $ map snd xs)) $
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
            , -- RFC 4035 Sec 2.3: The type bitmap of every NSEC
              -- resource record in a signed zone MUST indicate the
              -- presence of both the NSEC record itself and its
              -- corresponding RRSIG record.
              rdata = rd_nsec nxt (NSEC : RRSIG : types) -- putNsecTypes sorts this.
            }

makeNSECforSecondary
    :: M.Map (Domain, TYPE) ResourceRecord
    -> [ResourceRecord]
    -> [RRSetSig]
makeNSECforSecondary db rrs0 = map (bindSIG db) $ map (: []) rrs0

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

newtype NSECDB = NSECDB (M.Map DomainRange RRSetSig) deriving (Eq, Show)

lookupN :: Domain -> DB -> Maybe RRSetSig
lookupN dom db = M.lookup key nsecdb
  where
    key = Exact dom
    NSECDB nsecdb = dbNsecMap db

emptyNSECDB :: NSECDB
emptyNSECDB = NSECDB M.empty

makeNSECDB :: [RRSetSig] -> NSECDB
makeNSECDB vals = NSECDB $ M.fromList $ zip keys vals
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

----------------------------------------------------------------

data Node = Node
    { nodeMap :: M.Map Label Node
    , nodeRRs :: [RRSetSig]
    , nodeDelegated :: Bool
    }
    deriving (Show)

emptyNode :: Node
emptyNode = Node M.empty [] False

makeNode :: Int -> [RRSetSig] -> Node
makeNode n rrs0 = foldr (\(ls, ts) node -> insert ls ts node) emptyNode kvs
  where
    rrs :: [RRSetSig]
    rrs = nub $ sort rrs0
    rrss :: [[RRSetSig]]
    rrss = groupBy ((==) `on` rrsetsigName) rrs
    kvs :: [([Label], [RRSetSig])]
    kvs = map (\xs -> (getLabels xs, xs)) rrss
    getLabels xs = drop n $ revLabels $ rrsetsigName $ unsafeHead xs

checkDelegated :: [RRSetSig] -> Bool
checkDelegated rrs = any (\x -> rrsetsigType x `elem` [NS, DS]) rrs

insert :: [Label] -> [RRSetSig] -> Node -> Node
insert [] rrs node = node{nodeRRs = rrs}
insert [l] rrs node@Node{..} =
    let n = case M.lookup l nodeMap of
            Nothing -> emptyNode
            Just n0 -> n0
        deleg = checkDelegated rrs
        n' = n{nodeRRs = rrs, nodeDelegated = deleg}
        m' = M.insert l n' nodeMap
        node' = node{nodeMap = m'}
     in node'
insert (l : ls) rrs node@Node{..} =
    let n = case M.lookup l nodeMap of
            Nothing -> emptyNode
            Just n0 -> n0
        n' = insert ls rrs n
        m' = M.insert l n' nodeMap
        node' = node{nodeMap = m'}
     in node'

data Result
    = Exist [RRSetSig]
    | Deleg [RRSetSig] (Maybe [RRSetSig])
    | NX

lookupDB :: Domain -> DB -> Result
lookupDB dom DB{..} = loop ls0 dbNode Nothing
  where
    ls0 = drop dbLabelsCount $ revLabels dom
    loop [] node Nothing = Exist $ nodeRRs node
    loop [] node (Just x) = Deleg (nodeRRs x) $ Just $ nodeRRs node
    loop (l : ls) node mx = case M.lookup l $ nodeMap node of
        -- If l is "*" and "*" exist, match here.
        Just node'
            | nodeDelegated node' -> loop ls node' $ Just node'
            | otherwise -> loop ls node' mx
        Nothing
            | l /= "*" -> case M.lookup "*" $ nodeMap node of
                Nothing -> case mx of
                    Nothing -> NX
                    Just cut -> Deleg (nodeRRs cut) Nothing
                -- fixme deleg
                Just node' -> Exist $ map (synthesize dom) $ nodeRRs node'
            | otherwise -> case mx of
                Nothing -> NX
                Just cut -> Deleg (nodeRRs cut) Nothing

----------------------------------------------------------------

data CNAMECheck = Canon | Alias Domain [ResourceRecord] | CNErr

checkCNAME :: Bool -> [RRSetSig] -> CNAMECheck
checkCNAME dnssecOK rrs = case filter (\x -> rrsetsigType x == CNAME) rrs of
    [] -> Canon
    [rr] -> case rrsetsigRRs rr of
        [c] -> case fromRData $ rdata c of
            Nothing -> CNErr
            Just cname ->
                let cc = getRRs dnssecOK rr
                    dom = cname_domain cname
                 in Alias dom cc
        _ -> CNErr
    _ -> CNErr

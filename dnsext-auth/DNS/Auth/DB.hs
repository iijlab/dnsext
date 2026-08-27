{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Auth.DB (
    RRSetSig (..),
    DB (..),
    dbRD_SOA,
    dbSOArr,
    getRRs,
    AuthException (..),
    loadDB,
    makeDBforPrimary,
    makeDBforSecondary,
    emptyDB,
    loadZoneFile,
    NSECDB,
    lookupN,
    lookupN',
    DomainRange (..),
    Result (..),
    lookupDB,
    decideNXWildcard,
    NSEC3Proof (..),
    decideNSEC3Proof,
    CNAMECheck (..),
    checkCNAME,
) where

import qualified Control.Exception as E
import qualified Data.ByteString.Short as Short
import Data.Either
import Data.Function (on)
import Data.List (groupBy, nub, partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import GHC.Stack

import DNS.SEC
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque
import qualified DNS.ZoneFile as ZF

----------------------------------------------------------------

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
    , dbNsec3conv :: Maybe (Domain -> Domain)
    }

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
        { dbZone = zone
        , dbLabelsCount = 0
        , dbSOA = (soa, soarrsetsig)
        , dbNode = emptyNode zone
        , dbAll = []
        , dbNsecMap = emptyNSECDB
        , dbNsec3conv = Nothing
        }
  where
    zone = "."
    soard = rd_soa zone "." 0 0 0 0 0
    soa = fromJust $ fromRData soard
    soarr =
        ResourceRecord
            { rrname = zone
            , rrtype = SOA
            , rrclass = IN
            , rrttl = 0
            , rdata = soard
            }
    soarrsetsig =
        RRSetSig
            { rrsetsigName = zone
            , rrsetsigType = SOA
            , rrsetsigRRs = [soarr]
            , rrsetsigSig = Nothing
            }

----------------------------------------------------------------

newtype AuthException = AuthException String deriving (Eq, Show)

instance E.Exception AuthException

----------------------------------------------------------------

-- | This function throws 'AuthException'.
loadDB :: Domain -> FilePath -> IO DB
loadDB zone file = do
    rss <- loadZoneFile zone file
    makeDBforSecondary zone $ filter (\r -> rrtype r /= DS) rss

loadZoneFile :: Domain -> FilePath -> IO [ResourceRecord]
loadZoneFile zone file = catMaybes . map fromResource <$> ZF.parseFile file zone

----------------------------------------------------------------

-- | This function throws 'AuthException'.
makeDBforPrimary
    :: Domain
    -> (Maybe RD_NSEC3PARAM)
    -> (Bool -> [ResourceRecord] -> IO [RRSetSig])
    -> [ResourceRecord]
    -> IO DB
makeDBforPrimary _ _ _ [] = E.throwIO $ AuthException "No resource records"
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
makeDBforPrimary zone mn3p doSign (soarr : rrs)
    | rrtype soarr /= SOA = E.throwIO $ AuthException "SOA does not exist"
    | otherwise = case fromRData $ rdata soarr of
        Nothing -> E.throwIO $ AuthException "SOA is broken"
        Just soa -> do
            let ttl = soa_minimum soa
            let (is, ns, ds, gs, _os) = divide zone rrs
            ssSigned <- doSign True [soarr]
            isSigned <- doSign True is
            dsSigned <- doSign True ds
            n3pSigned <- case mn3p of
                Nothing -> return []
                Just n3p -> do
                    let n3prr =
                            ResourceRecord
                                { rrname = zone
                                , rrtype = NSEC3PARAM
                                , rdata = toRData n3p
                                , rrclass = IN
                                , rrttl = ttl -- fixme
                                }
                    doSign True [n3prr]
            -- In-domain NS/DS should have NSEC.
            node <- makeNode zone (ssSigned ++ n3pSigned ++ isSigned ++ unsign ns ++ dsSigned ++ unsign gs)
            (nsecSigned, nsecdb, mconv) <- case mn3p of
                Nothing -> do
                    xs <- makeNSECforPrimary ttl doSign node
                    let ndb = makeNSECDB xs
                    return (xs, ndb, Nothing)
                Just n3p -> do
                    xs <- makeNSEC3forPrimary ttl zone doSign n3p node
                    let ndb = makeNSEC3DB zone xs
                        conv = hashedDomain zone n3p
                    return (xs, ndb, Just conv)
            let allrr =
                    getRRs True (unsafeHead ssSigned)
                        ++ concatMap (getRRs True) isSigned
                        ++ concatMap (getRRs True) n3pSigned
                        ++ ns
                        ++ concatMap (getRRs True) dsSigned
                        ++ concatMap (getRRs True) nsecSigned
                        ++ gs
                        ++ _os
                        ++ [soarr] -- for AXFR
                db = makeDBFinal zone soa ssSigned node allrr nsecdb mconv
            return db

-- | This function throws 'AuthException'.
makeDBforSecondary :: Domain -> [ResourceRecord] -> IO DB
makeDBforSecondary _ [] = E.throwIO $ AuthException "No resource records"
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
makeDBforSecondary zone (soarr : rrs0)
    | rrtype soarr /= SOA = E.throwIO $ AuthException "SOA does not exist"
    | otherwise = case fromRData $ rdata soarr of
        Nothing -> E.throwIO $ AuthException "SOA is broken"
        Just soa -> do
            let (sigs, rrs1) = partition (\r -> rrtype r == RRSIG) rrs0
                nsec3params = filter (\r -> rrtype r == NSEC3PARAM) rrs0
                (nsec, rrs)
                    | null nsec3params = partition (\r -> rrtype r == NSEC) rrs1
                    | otherwise = partition (\r -> rrtype r == NSEC3) rrs1
            let (is, ns, ds, gs, _os) = divide zone rrs
                sigDB = M.fromList $ catMaybes $ map rrsigKV sigs
                ssSigned = groupAndSig sigDB [soarr]
                isSigned = groupAndSig sigDB is
                dsSigned = groupAndSig sigDB ds
            node <- makeNode zone (ssSigned ++ isSigned ++ unsign ns ++ dsSigned ++ unsign gs)
            let nsecSigned = makeNSECforSecondary sigDB nsec
                nsecdb
                    | null nsec3params = makeNSECDB nsecSigned
                    | otherwise = makeNSEC3DB zone nsecSigned
            let allrr = [soarr] ++ rrs ++ [soarr] -- for AXFR
                mconv = case nsec3params of
                    [] -> Nothing
                    nsec3param : _ -> case fromRData $ rdata nsec3param of
                        Nothing -> Nothing
                        Just n3p -> Just $ hashedDomain zone n3p
                db = makeDBFinal zone soa ssSigned node allrr nsecdb mconv
            return db

----------------------------------------------------------------

makeDBFinal
    :: Domain
    -> RD_SOA
    -> [RRSetSig]
    -> Node
    -> [ResourceRecord]
    -> NSECDB
    -> Maybe (Domain -> Domain)
    -> DB
makeDBFinal zone soa ssSigned node allrr nsecdb mconv =
    DB
        { dbZone = zone
        , dbLabelsCount = n
        , dbSOA = (soa, unsafeHead ssSigned)
        , dbNode = node
        , dbAll = allrr
        , dbNsecMap = nsecdb
        , dbNsec3conv = mconv
        }
  where
    n = labelsCount zone

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
-- Empty non terminals are not included in rrs because they
-- are from a zone file.
--
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
    :: TTL
    -> (Bool -> [ResourceRecord] -> IO [RRSetSig])
    -> Node
    -> IO [RRSetSig]
makeNSECforPrimary ttl doSign root = doSign False $ map pack zipped
  where
    packedNameTypes :: [(Domain, [TYPE])]
    packedNameTypes = foldNode skipENTandUnderDelegated root
    h = unsafeHead packedNameTypes
    slided = drop 1 packedNameTypes ++ [h]
    zipped :: [((Domain, [TYPE]), (Domain, [TYPE]))]
    zipped = zip packedNameTypes slided
    pack ((dom, types), (nxt, _)) =
        ResourceRecord
            { rrname = dom
            , rrclass = IN
            , rrtype = NSEC
            , rrttl = ttl
            , -- RFC 4035 Sec 2.3: The type bitmap of every NSEC
              -- resource record in a signed zone MUST indicate the
              -- presence of both the NSEC record itself and its
              -- corresponding RRSIG record.
              rdata = rd_nsec nxt (NSEC : RRSIG : types) -- putNsecTypes sorts this.
            }
    skipENTandUnderDelegated Node{..} = (xs, not nodeDelegated)
      where
        types = map rrsetsigType nodeRRs
        xs
            | null types = []
            | otherwise = [(nodeName, types)]

----------------------------------------------------------------

n3hash :: RD_NSEC3PARAM -> Domain -> Opaque
n3hash n3p x = fromRight (error "n3hash") $ hashNSEC3PARAM n3p x

encB32 :: Opaque -> Short.ShortByteString
encB32 = Short.toShort . Opaque.toBase32Hex

hashedDomain :: Domain -> RD_NSEC3PARAM -> Domain -> Domain
hashedDomain zone n3p d = label `consDomain` zone
  where
    label = encB32 $ n3hash n3p d

expandHashedLabel :: Opaque -> Domain -> Domain
expandHashedLabel l zone = encB32 l `consDomain` zone

makeNSEC3forPrimary
    :: TTL
    -> Domain
    -> (Bool -> [ResourceRecord] -> IO [RRSetSig])
    -> RD_NSEC3PARAM
    -> Node
    -> IO [RRSetSig]
makeNSEC3forPrimary ttl zone doSign n3p@RD_NSEC3PARAM{..} root = doSign False $ map pack zipped
  where
    packedNameTypes :: [(Domain, [TYPE])]
    packedNameTypes = foldNode skipUnderDelegated root
    -- not base32
    hashedNameTypes = sort $ map (\(d, ts) -> (n3hash n3p d, ts)) $ packedNameTypes
    h = unsafeHead hashedNameTypes
    slided = drop 1 hashedNameTypes ++ [h]
    zipped :: [((Opaque, [TYPE]), (Opaque, [TYPE]))]
    zipped = zip hashedNameTypes slided
    pack ((hashedLbl, types), (nxt, _)) =
        ResourceRecord
            { rrname = expandHashedLabel hashedLbl zone
            , rrclass = IN
            , rrtype = NSEC3
            , rrttl = ttl
            , rdata = rd_nsec3 nsec3param_hashalg [] nsec3param_iterations nsec3param_salt nxt (RRSIG : types)
            }
    skipUnderDelegated Node{..} = (xs, not nodeDelegated)
      where
        types = map rrsetsigType nodeRRs
        xs
            -- fixme: OptOut flag
            | nodeDelegated = if DS `elem` types then [(nodeName, types)] else []
            | otherwise = [(nodeName, types)]

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

lookupN :: Domain -> DB -> [ResourceRecord]
lookupN dom db = case M.lookup key nsecdb of
    Nothing -> []
    Just n -> getRRs True n
  where
    key = Exact dom
    NSECDB nsecdb = dbNsecMap db

lookupN' :: Domain -> DB -> [ResourceRecord]
lookupN' dom db = case M.lookup key nsecdb of
    Nothing -> []
    Just n
        | rrsetsigName n == dom -> getRRs True n
        | otherwise -> []
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

    -- jp, aa.jp, bb.jp, cc.jp, jp ->
    -- jp, aa.jp, bb.jp, cc.jp, jp0
    zero :: Short.ShortByteString
    zero = "\x00"
    modify :: Domain -> Domain
    modify dom = case toWireLabels dom of
        [] -> fromWireLabels [zero]
        l : ls -> fromWireLabels (l <> zero : ls)

makeNSEC3DB :: Domain -> [RRSetSig] -> NSECDB
makeNSEC3DB zone vals = NSECDB $ M.fromList $ zip keys (last vals : vals)
  where
    keys = modifyHead $ modifyTail $ catMaybes $ map unpack vals
    unpack :: RRSetSig -> Maybe (Domain, Domain)
    unpack rss = case fromRData $ rdata r of
        Nothing -> Nothing
        Just nsec3 ->
            let label = Opaque.toBase32Hex $ fromNSEC3Next $ nsec3_next_hashed_owner_name nsec3
                next = consDomain (Short.toShort label) zone
             in Just (rrsetsigName rss, next)
      where
        r = unsafeHead $ rrsetsigRRs rss

    --     00.jp, 11,jp, 22.jp, 33.jp, 00.jp
    -- jp, 00.jp, 11,jp, 22.jp, 33.jp, 00.jp0
    modifyHead ls@((Range x _) : _) = Range zone x : ls
    modifyHead ls = ls -- never reach
    modifyTail [] = []
    modifyTail [(x, _)] = [Range x modifiedZone]
    modifyTail ((x, y) : xys) = Range x y : modifyTail xys

    zero :: Short.ShortByteString
    zero = "\x00"
    modifiedZone :: Domain
    modifiedZone = case toWireLabels zone of
        l : ls -> fromWireLabels (l <> zero : ls)
        _ -> zone -- never reach

----------------------------------------------------------------

data Node = Node
    { nodeName :: Domain
    , nodeMap :: M.Map Label Node
    , nodeRRs :: [RRSetSig]
    , nodeDelegated :: Bool
    , nodeHasDS :: Bool
    }
    deriving (Show)

emptyNode :: Domain -> Node
emptyNode dom =
    Node
        { nodeName = dom
        , nodeMap = M.empty
        , nodeRRs = []
        , nodeDelegated = False
        , nodeHasDS = False
        }

makeNode :: Domain -> [RRSetSig] -> IO Node
makeNode zone rrs0 = do
    let kvs = makeRRSetSigGroup zone rrs0
    mapM_ checkRRSetSigGroup kvs
    return $ fromRRSetSigGroup zone kvs

makeRRSetSigGroup :: Domain -> [RRSetSig] -> [((Domain, [Label]), [RRSetSig])]
makeRRSetSigGroup zone rrs0 = map (\xs -> (getLabels xs, xs)) rrss
  where
    n = labelsCount zone
    rrs :: [RRSetSig]
    rrs = nub $ sort rrs0
    rrss :: [[RRSetSig]]
    rrss = groupBy ((==) `on` rrsetsigName) rrs
    getLabels xs = (name, ls)
      where
        name = rrsetsigName $ unsafeHead xs
        ls = drop n $ revLabels name

fromRRSetSigGroup :: Domain -> [((Domain, [Label]), [RRSetSig])] -> Node
fromRRSetSigGroup zone kvs =
    foldr (\((name, ls), ts) node -> insert ls name ts node) zoneRoot kvs
  where
    zoneRoot = emptyNode zone

checkRRSetSigGroup :: ((Domain, [Label]), [RRSetSig]) -> IO ()
checkRRSetSigGroup (dl, rrs)
    | any (\x -> rrsetsigType x == CNAME) rrs = case rrs of
        [c]
            | length (rrsetsigRRs c) /= 1 ->
                E.throwIO $ AuthException "Multiple CNAME"
            | otherwise ->
                return ()
        _ -> E.throwIO $ AuthException $ "CNAME with other RRs: " ++ show dl
    | otherwise = return ()

checkDelegated :: [RRSetSig] -> Bool
checkDelegated rrs = any (\x -> rrsetsigType x == NS) rrs

checkDS :: [RRSetSig] -> Bool
checkDS rrs = any (\x -> rrsetsigType x == DS) rrs

insert :: [Label] -> Domain -> [RRSetSig] -> Node -> Node
insert [] _ rrs node = node{nodeRRs = rrs}
insert [l] dom rrs node@Node{..} =
    let n = case M.lookup l nodeMap of
            Nothing -> emptyNode dom
            Just n0 -> n0
        deleg = checkDelegated rrs
        n' =
            n
                { nodeRRs = rrs
                , nodeDelegated = deleg
                , nodeHasDS = if deleg then checkDS rrs else False
                }
        m' = M.insert l n' nodeMap
        node' = node{nodeMap = m'}
     in node'
insert (l : ls) dom rrs node@Node{..} =
    let n = case M.lookup l nodeMap of
            -- Empty non terminal
            Nothing ->
                let nm = fromWireLabels $ drop (length ls) $ wireLabels dom
                 in emptyNode nm
            Just n0 -> n0
        n' = insert ls dom rrs n
        m' = M.insert l n' nodeMap
        node' = node{nodeMap = m'}
     in node'

data Result
    = Exist [RRSetSig] (Maybe Domain) -- wildcard
    | Deleg [RRSetSig] (Maybe [RRSetSig])
    | NonEx

lookupDB :: Domain -> DB -> Result
lookupDB dom DB{..} = loop ls0 dbNode Nothing
  where
    rls = revLabels dom
    ls0 = drop dbLabelsCount $ revLabels dom
    loop [] node Nothing = Exist (nodeRRs node) Nothing
    loop [] node (Just x) = Deleg (nodeRRs x) $ Just $ nodeRRs node
    loop (l : ls) node mx = case M.lookup l $ nodeMap node of
        -- If l is "*" and "*" exist, match here.
        Just node'
            | nodeDelegated node' -> loop ls node' $ Just node'
            | otherwise -> loop ls node' mx
        Nothing
            | l /= "*" -> case M.lookup "*" $ nodeMap node of
                Nothing -> case mx of
                    Nothing -> NonEx
                    Just cut -> Deleg (nodeRRs cut) Nothing
                Just node' ->
                    let len = labelsCount dom - length ls - 1
                        wild = fromWireLabels ("*" : reverse (take len rls))
                     in Exist (map (synthesize dom) $ nodeRRs node') $ Just wild
            | otherwise -> case mx of
                Nothing -> NonEx
                Just cut -> Deleg (nodeRRs cut) Nothing

decideNXWildcard :: Domain -> DB -> Maybe Domain
decideNXWildcard dom DB{..} = loop ls0 dbNode
  where
    rls :: [Label]
    rls = revLabels dom
    ls0 = drop dbLabelsCount $ revLabels dom
    loop :: [Label] -> Node -> Maybe Domain
    loop [] _ = Nothing -- Exist
    loop (l : ls) node = case M.lookup l $ nodeMap node of
        Nothing ->
            let len = labelsCount dom - length ls - 1
                closest = reverse $ take len rls
                wild = fromWireLabels ("*" : closest)
             in Just wild
        Just node'
            | nodeDelegated node' -> Nothing
            | otherwise -> loop ls node'

data NSEC3Proof = NSEC3Proof
    { closestEncloser :: Domain
    , proofNextCloser :: Domain
    , proofWildcardCloser :: Domain
    }

decideNSEC3Proof :: Domain -> DB -> Maybe NSEC3Proof
decideNSEC3Proof dom DB{..} = loop ls0 dbNode
  where
    rls :: [Label]
    rls = revLabels dom
    ls0 = drop dbLabelsCount $ revLabels dom
    loop :: [Label] -> Node -> Maybe NSEC3Proof
    loop [] _ = Nothing -- Exist
    loop (l : ls) node = case M.lookup l $ nodeMap node of
        Nothing -> Just $ makeProof l ls
        Just node'
            | nodeDelegated node' && nodeHasDS node' ->
                let proof = makeProof l ls
                 in -- proofNextCloser etc are dummy
                    Just $ proof{closestEncloser = proofNextCloser proof}
            | nodeDelegated node' -> Just $ makeProof l ls
            | otherwise -> loop ls node'
    makeProof l ls =
        NSEC3Proof
            { closestEncloser = encloser
            , proofNextCloser = next
            , proofWildcardCloser = wild
            }
      where
        len = labelsCount dom - length ls - 1
        closest = reverse $ take len rls
        encloser = fromWireLabels closest
        next = fromWireLabels (l : closest)
        wild = fromWireLabels ("*" : closest)

----------------------------------------------------------------

foldNode :: (Node -> ([a], Bool)) -> Node -> [a]
foldNode extract root = walk root
  where
    walk n = xs ++ if deeper then dig (nodeMap n) else []
      where
        (xs, deeper) = extract n
    dig m = M.foldr (\n xs -> walk n ++ xs) [] m

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

{-# LANGUAGE OverloadedStrings #-}

module DNS.Auth.DB (
    DB (..),
    loadDB,
    makeDB,
    emptyDB,
) where

import Data.Function (on)
import Data.List (groupBy, partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set

import DNS.Types
import qualified DNS.ZoneFile as ZF

----------------------------------------------------------------

data DB = DB
    { dbZone :: Domain
    , dbSOA :: RD_SOA
    , dbSOArr :: ResourceRecord
    , dbAnswer :: M.Map Domain [ResourceRecord]
    , dbAuthority :: M.Map Domain [ResourceRecord]
    , dbAdditional :: M.Map Domain [ResourceRecord]
    , dbAll :: [ResourceRecord]
    }
    deriving (Show)

emptyDB :: DB
emptyDB =
    DB
        { dbZone = "."
        , dbSOA = soa
        , dbSOArr = soarr
        , dbAnswer = M.empty
        , dbAuthority = M.empty
        , dbAdditional = M.empty
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
                    , dbSOA = soa
                    , dbSOArr = soarr
                    , dbAnswer = ans
                    , dbAuthority = auth
                    , dbAdditional = add
                    , dbAll = [soarr] ++ rrs ++ [soarr] -- for AXFR
                    }
  where
    -- RFC 9471
    -- In-domain and sibling glues only.
    -- Unrelated glues are ignored.
    (as, ns, _os) = partition3 zone rrs
    isDelegated = makeIsDelegated ns
    (gs, zs) = partition (\r -> isDelegated (rrname r)) as
    -- gs: glue (in delegated domain)
    -- zs: in-domain
    -- expand is for RFC 4592 Sec 2.2.2.Empty Non-terminals
    ans = makeMap $ [soarr] ++ concat (map (expand zone) zs)
    auth = makeMap ns
    xs = filter (\r -> rrtype r == A || rrtype r == AAAA) zs
    add = makeMap $ xs ++ gs

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

makeIsDelegated :: [ResourceRecord] -> (Domain -> Bool)
makeIsDelegated rrs = \dom -> or (map (\f -> f dom) ps)
  where
    s = Set.fromList $ map rrname rrs
    ps = map (\x -> (`isSubDomainOf` x)) $ Set.toList s

makeMap :: [ResourceRecord] -> M.Map Domain [ResourceRecord]
makeMap rrs = M.fromList kvs
  where
    ts = groupBy ((==) `on` rrname) $ sort rrs
    vs = map (filter (\rr -> rrtype rr /= NULL)) ts
    ks = map (rrname . unsafeHead) ts
    kvs = zip ks vs

unsafeHead :: [a] -> a
unsafeHead (x : _) = x
unsafeHead _ = error "unsafeHead"

----------------------------------------------------------------

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

-- For RFC 4592 Sec 2.2.2.Empty Non-terminals
expand :: Domain -> ResourceRecord -> [ResourceRecord]
expand dom rr = loop r0
  where
    r0 = rrname rr
    loop r
        | r == dom = [rr]
        | otherwise = case unconsDomain r of
            Nothing -> [rr]
            Just (_, r1) -> rrnull r : loop r1

rrnull :: Domain -> ResourceRecord
rrnull r =
    ResourceRecord
        { rrname = r
        , rrtype = NULL
        , rrclass = IN
        , rrttl = 0
        , rdata = rd_null ""
        }

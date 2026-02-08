module DNS.Auth.DB (
    DB (..),
    RRsets (..),
    loadDB,
) where

import Data.Function (on)
import Data.List (groupBy, partition, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

import DNS.Types
import qualified DNS.ZoneFile as ZF

----------------------------------------------------------------

data DB = DB
    { dbZone :: Domain
    , dbSOA :: ResourceRecord
    , dbMap :: M.Map Domain RRsets
    , dbGlue :: M.Map Domain [ResourceRecord]
    }

data RRsets = RRsets
    { rrsetA :: [ResourceRecord]
    , rrsetAAAA :: [ResourceRecord]
    , rrsetNS :: [ResourceRecord]
    , rrsetOthers :: [ResourceRecord]
    }

----------------------------------------------------------------

loadDB :: String -> FilePath -> IO (Either String DB)
loadDB zone file = make <$> loadZoneFile zone file

loadZoneFile :: String -> FilePath -> IO (Domain, [ResourceRecord])
loadZoneFile zone file = do
    rrs <- catMaybes . map fromResource <$> ZF.parseFile' file dom
    return (dom, rrs)
  where
    dom = fromRepresentation zone

----------------------------------------------------------------

make :: (Domain, [ResourceRecord]) -> Either String DB
make (_, []) = Left "make: no resource records"
-- RFC 1035 Sec 5.2
-- Exactly one SOA RR should be present at the top of the zone.
make (zone, soa : rrs)
    | rrtype soa /= SOA = Left "make: no SOA"
    | otherwise =
        Right $
            DB
                { dbZone = zone
                , dbSOA = soa
                , dbMap = m
                , dbGlue = g
                }
  where
    -- RFC 9471
    -- In-domain and sibling glues only.
    -- Unrelated glues are ignored.
    (as, ns, _os) = partition3 zone rrs
    isDelegated = makeIsDelegated ns
    (glue, inzone) = partition (\r -> isDelegated (rrname r)) as
    m = makeMap makeRRsets $ [soa] ++ ns ++ inzone
    g = makeMap id glue

partition3 :: Domain -> [ResourceRecord] -> ([ResourceRecord], [ResourceRecord], [ResourceRecord])
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

makeMap :: ([ResourceRecord] -> v) -> [ResourceRecord] -> M.Map Domain v
makeMap conv rrs = M.fromList kvs
  where
    gs = groupBy ((==) `on` rrname) $ sort rrs
    ks = map (rrname . unsafeHead) gs
    vs = map conv gs
    kvs = zip ks vs

unsafeHead :: [a] -> a
unsafeHead (x : _) = x
unsafeHead _ = error "unsafeHead"

----------------------------------------------------------------

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

makeRRsets :: [ResourceRecord] -> RRsets
makeRRsets rs0 =
    let (as, aaaas, nss, others) = loop id id id id rs0
     in RRsets
            { rrsetA = as
            , rrsetAAAA = aaaas
            , rrsetNS = nss
            , rrsetOthers = others
            }
  where
    loop a b c d [] = (a [], b [], c [], d [])
    loop a b c d (r : rs) = case rrtype r of
        A -> loop (a . (r :)) b c d rs
        AAAA -> loop a (b . (r :)) c d rs
        NS -> loop a b (c . (r :)) d rs
        _ -> loop a b c (d . (r :)) rs

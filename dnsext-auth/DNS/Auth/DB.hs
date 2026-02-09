module DNS.Auth.DB (
    DB (..),
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
    , dbAnswer :: M.Map Domain [ResourceRecord]
    , dbAuthority :: M.Map Domain [ResourceRecord]
    , dbAdditional :: M.Map Domain [ResourceRecord]
    }
    deriving (Show)

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
                , dbAnswer = ans
                , dbAuthority = auth
                , dbAdditional = add
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
    ans = makeMap $ [soa] ++ zs
    auth = makeMap ns
    add = makeMap gs

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
    vs = groupBy ((==) `on` rrname) $ sort rrs
    ks = map (rrname . unsafeHead) vs
    kvs = zip ks vs

unsafeHead :: [a] -> a
unsafeHead (x : _) = x
unsafeHead _ = error "unsafeHead"

----------------------------------------------------------------

fromResource :: ZF.Record -> Maybe ResourceRecord
fromResource (ZF.R_RR r) = Just r
fromResource _ = Nothing

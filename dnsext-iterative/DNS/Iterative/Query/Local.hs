module DNS.Iterative.Query.Local where

-- dnsext packages
import DNS.Types

-- this package

import DNS.Iterative.Query.Class
import DNS.Iterative.Query.LocalZone (lookupApex, lookupName)

{- FOURMOLU_DISABLE -}
takeLocalResult :: Env -> Question -> a -> a -> (ResultRRS -> a) -> a
takeLocalResult env q@Question{qname=dom,qclass=cls} denied nothing just
    | CH <- cls, (apexes, names) <- chaosZones_ env, Just apex <- lookupApex apexes dom = maybe denied just $ lookupName names apex q
    | IN <- cls, (apexes, names) <- localZones_ env, Just apex <- lookupApex apexes dom = maybe denied just $ lookupName names apex q
    | otherwise                                                                         = nothing
{- FOURMOLU_ENABLE -}

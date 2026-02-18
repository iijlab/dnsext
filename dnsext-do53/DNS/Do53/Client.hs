module DNS.Do53.Client (
    -- * Lookups returning each type
    lookupA,
    lookupAAAA,
    lookupMX,
    lookupAviaMX,
    lookupAAAAviaMX,
    lookupNS,
    lookupNSAuth,
    lookupTXT,
    lookupSOA,
    lookupPTR,
    lookupRDNS,
    lookupSRV,

    -- * Lookups returning requested RData
    lookup,
    lookupAuth,

    -- * Lookups returning requested resource data
    lookupX,
    lookupAuthX,

    -- * Lookups returning DNS Messages
    lookupRaw,

    -- * Lookup configuration for sub resolvers
    LookupConf,
    defaultLookupConf,
    withLookupConf,
    UDPRetry,
    LookupEnv,

    -- ** Accessors
    lconfSeeds,
    lconfUDPRetry,
    lconfVCLimit,
    lconfConcurrent,
    lconfCacheConf,
    lconfQueryControls,
    lconfActions,

    -- ** Specifying full resolvers
    Seeds (..),

    -- ** Configuring cache
    CacheConf,
    defaultCacheConf,
    maximumTTL,
    pruningDelay,

    -- ** Actions
    ResolveActions,
    defaultResolveActions,
    ractionTimeoutTime,
    ractionGenId,
    ractionGetTime,
    ractionLog,
    ractionShortLog,
    Reply,

    -- ** Query control
    QueryControls (..),
    FlagControls (..),
    EdnsControls (..),
    FlagOp (..),
    rdFlag,
    adFlag,
    cdFlag,
    doFlag,
    ednsEnabled,
    ednsSetVersion,
    ednsSetUdpSize,
    ednsSetOptions,
    ODataOp (..),
)
where

import DNS.Do53.Lookup
import DNS.Do53.LookupX
import DNS.Do53.Query
import DNS.Do53.Types
import Prelude hiding (lookup)

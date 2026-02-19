module DNS.Do53.Internal (
    -- * TCP related
    openTCP,
    sendTCP,
    recvTCP,

    -- * Virtual circuit
    sendVC,
    recvVC,
    encodeVCLength,
    decodeVCLength,
    BS,

    -- * Resolver
    Resolver,
    Reply (..),

    -- * Pipeline resolver
    PersistentResolver,
    PipelineResolver,
    tcpPersistentResolver,
    vcPersistentResolver,

    -- * One-shot resolver
    OneshotResolver,
    udpTcpResolver,
    udpResolver,
    tcpResolver,
    vcResolver,

    -- * Resolver information
    ResolveInfo (..),
    defaultResolveInfo,
    UDPRetry,
    VCLimit (..),
    ResolveActions (..),
    defaultResolveActions,

    -- * One shot resolve function
    resolve,
    ResolveEnv (..),

    -- * Query
    encodeQuery,
    modifyQuery,
    queryControls,
    CodeControls (..),
    opCode,
    aaFlag,

    -- * Generating identifier
    singleGenId,
    newConcurrentGenId,

    -- * Misc
    LookupEnv (..),
    checkRespM,
    withLookupConfAndResolver,
    NameTag (..),
    nameTag,
    fromNameTag,
    toNameTag,
    queryTag,
    raceAny,
)
where

import DNS.Do53.Do53
import DNS.Do53.IO
import DNS.Do53.Id
import DNS.Do53.Lookup
import DNS.Do53.Query
import DNS.Do53.Resolve
import DNS.Do53.Types
import DNS.Do53.VC

module DNS.Types.Internal (
    -- * Types
    TYPE (..),
    OptCode (..),
    RCODE (..),
    CanonicalFlag (..),
    CLASS (..),

    -- * Classes
    ResourceData (..),
    OptData (..),

    -- * Misc
    section,
    defaultQuestion,

    -- * Extension
    extendRR,
    extendOpt,

    -- * High level
    putDNSMessage,
    getDNSMessage,
    putDNSFlags,
    getDNSFlags,
    putQuestion,
    getQuestion,
    putResourceRecord,
    getResourceRecord,
    putRData,
    getRData,

    -- * Middle level
    putDomain,
    putDomainRFC1035,
    getDomain,
    getDomainRFC1035,
    putMailbox,
    putMailboxRFC1035,
    getMailbox,
    getMailboxRFC1035,
    putOpaque,
    putLenOpaque,
    getOpaque,
    getLenOpaque,
    putTYPE,
    getTYPE,
    putCLASS,
    getCLASS,
    putSeconds,
    getSeconds,

    -- * Low level
    module DNS.Wire,
) where

import DNS.Types.Dict
import DNS.Types.Domain
import DNS.Types.EDNS
import DNS.Types.Message
import DNS.Types.Opaque.Internal
import DNS.Types.RData
import DNS.Types.Seconds
import DNS.Types.Type
import DNS.Wire

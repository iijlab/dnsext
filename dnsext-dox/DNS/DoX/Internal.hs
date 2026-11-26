module DNS.DoX.Internal (
    http2Resolver,
    http2PersistentResolver,
    http2cResolver,
    http2cPersistentResolver,
    http3Resolver,
    http3PersistentResolver,
    tlsResolver,
    tlsPersistentResolver,
    quicResolver,
    quicPersistentResolver,
    encodeTLSResumption,
    decodeTLSResumption,
)
where

import DNS.DoX.HTTP2
import DNS.DoX.HTTP3
import DNS.DoX.QUIC
import DNS.DoX.TLS

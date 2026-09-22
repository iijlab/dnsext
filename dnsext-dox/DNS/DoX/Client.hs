{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.DoX.Client (
    -- * SVCB information
    lookupSVCBInfo,
    svcbResolveInfos,
    SVCBInfo (..),
    modifyForDDR,

    -- * Pipeline resolver
    toPipelineResolver,
    toPipelineResolvers,
    makePersistentResolver,

    -- * Oneshot resolver
    toResolveEnvs,
    lookupRawDoX,
    makeOneshotResolver,

    -- * ALPN
    doxPort,
)
where

import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.DoX.Imports
import DNS.DoX.Internal
import qualified DNS.Log as Log
import DNS.SVCB
import DNS.Types
import qualified Data.List.NonEmpty as NE
import Network.Socket (PortNumber)

-- $setup
-- >>> :seti -XOverloadedStrings

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | From APLN to its port number.
--
-- >>> doxPort "dot"
-- 853
doxPort :: ALPN -> PortNumber
doxPort "dot"   = 853
doxPort "doq"   = 853
doxPort "h2"    = 443
doxPort "h2c"   =  80
doxPort "h3"    = 443
doxPort _       =  53

----------------------------------------------------------------

-- | Making resolver according to ALPN.
--
--  The third argument is a path for HTTP query.
makePersistentResolver :: ALPN -> Maybe PersistentResolver
makePersistentResolver "tcp" = Just tcpPersistentResolver
makePersistentResolver "dot" = Just tlsPersistentResolver
makePersistentResolver "doq" = Just quicPersistentResolver
makePersistentResolver "h2"  = Just http2PersistentResolver
makePersistentResolver "h2c" = Just http2cPersistentResolver
makePersistentResolver "h3"  = Just http3PersistentResolver
makePersistentResolver _     = Nothing

makeOneshotResolver :: ALPN -> Maybe OneshotResolver
makeOneshotResolver "tcp" = Just tcpResolver
makeOneshotResolver "dot" = Just tlsResolver
makeOneshotResolver "doq" = Just quicResolver
makeOneshotResolver "h2"  = Just http2Resolver
makeOneshotResolver "h2c" = Just http2cResolver
makeOneshotResolver "h3"  = Just http3Resolver
makeOneshotResolver _     = Nothing
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

-- | Looking up SVCB RR first and lookup the target automatically
--   according to the priority of the values of SVCB RR.
lookupRawDoX :: LookupEnv -> Question -> IO (Either DNSError Reply)
lookupRawDoX lenv@LookupEnv{..} q = do
    er <- lookupSVCBInfo lenv
    case er of
        Left err -> return $ Left err
        Right addss -> case toResolveEnvs <$> addss of
            [] -> return $ Left FormatError
            adds : _ -> case adds of
                [] -> return $ Left FormatError
                add : _ -> resolve add q lenvQueryControls

----------------------------------------------------------------

data SVCBInfo = SVCBInfo
    { svcbInfoALPN :: ALPN
    , svcbInfoNameTag :: NameTag
    , svcbInfoResolveInfos :: [ResolveInfo]
    }
    deriving (Show)

-- | Don't forget to call 'addResourceDataForSVCB'.
lookupSVCBInfo :: LookupEnv -> IO (Either DNSError [[SVCBInfo]])
lookupSVCBInfo lenv@LookupEnv{..} = do
    er <- lookupRaw lenv $ Question "_dns.resolver.arpa" SVCB IN
    case er of
        Left err -> return $ Left err
        Right res -> do
            let ss = extractSVCB res
                -- mainly to pass 'rinfoActions'
                ri = NE.head $ renvResolveInfos lenvResolveEnv
                addss = svcbResolveInfos (replyTag res) ri ss
            logIt lenv ss
            return $ Right addss

extractSVCB :: Reply -> [RD_SVCB]
extractSVCB Reply{..} = sort (extractResourceData Answer replyDNSMessage) :: [RD_SVCB]

logIt :: LookupEnv -> [RD_SVCB] -> IO ()
logIt LookupEnv{..} ss = ractionLog lenvActions Log.DEMO Nothing r
  where
    r = prettyShowRData . toRData <$> ss

----------------------------------------------------------------

toResolveEnvs :: [SVCBInfo] -> [ResolveEnv]
toResolveEnvs sis = mapMaybe toResolveEnv sis

toResolveEnv :: SVCBInfo -> Maybe ResolveEnv
toResolveEnv SVCBInfo{..}
    | null svcbInfoResolveInfos = Nothing
    | otherwise = case makeOneshotResolver svcbInfoALPN of
        Nothing -> Nothing
        Just resolver -> Just $ ResolveEnv resolver True $ NE.fromList svcbInfoResolveInfos

toPipelineResolvers :: [SVCBInfo] -> [[PipelineResolver]]
toPipelineResolvers sis = toPipelineResolver <$> sis

toPipelineResolver :: SVCBInfo -> [PipelineResolver]
toPipelineResolver SVCBInfo{..}
    | null svcbInfoResolveInfos = []
    | otherwise = case makePersistentResolver svcbInfoALPN of
        Nothing -> []
        Just resolver -> resolver <$> svcbInfoResolveInfos

----------------------------------------------------------------

svcbResolveInfos :: NameTag -> ResolveInfo -> [RD_SVCB] -> [[SVCBInfo]]
svcbResolveInfos ntag ri ss = onPriority ntag ri <$> ss

onPriority :: NameTag -> ResolveInfo -> RD_SVCB -> [SVCBInfo]
onPriority ntag ri s = case extractSvcParam SPK_ALPN $ svcb_params s of
    Nothing -> []
    Just alpns -> onALPN ntag ri s <$> alpn_names alpns

onALPN :: NameTag -> ResolveInfo -> RD_SVCB -> ALPN -> SVCBInfo
onALPN ntag ri s alpn =
    SVCBInfo
        { svcbInfoALPN = alpn
        , svcbInfoNameTag = ntag
        , svcbInfoResolveInfos = extractResolveInfo ntag ri s alpn
        }

extractResolveInfo :: NameTag -> ResolveInfo -> RD_SVCB -> ShortByteString -> [ResolveInfo]
extractResolveInfo ntag ri s alpn = updateIPPort <$> ips
  where
    params = svcb_params s
    target = svcb_target s
    port = maybe (doxPort alpn) port_number $ extractSvcParam SPK_Port params
    mdohpath = dohpath <$> extractSvcParam SPK_DoHPath params
    v4s = case extractSvcParam SPK_IPv4Hint params of
        Nothing -> []
        Just v4 -> show <$> hint_ipv4s v4
    v6s = case extractSvcParam SPK_IPv6Hint params of
        Nothing -> []
        Just v6 -> show <$> hint_ipv6s v6
    ips = case v4s ++ v6s of
        [] -> [(nameTagIP ntag, port)] -- no "ipv4hint" nor "ipv6hint"
        xs -> (\h -> (fromString h, port)) <$> xs
    updateIPPort (x, y) =
        ri
            { rinfoIP = x
            , rinfoPort = y
            , rinfoPath = mdohpath
            , rinfoServerName = Just $ init $ toRepresentation target
            }

----------------------------------------------------------------

-- | Extracting 'ResolveInfo' from 'SVCBInfo' for DDR
--
-- RFC 9462 (Discovery of Designated Resolvers) requires the following
-- *designation*:
--
--   ipaddr -> SVCB(ipv4hint)
--   ipv4hint -> SAN(ipaddr)
--       SNI = None
--       Host: ipaddr
--
-- Sec 4.2 says:
--
-- 1. The client MUST verify the chain of certificates up to a trust
--    anchor as described in Section 6 of [RFC5280].  The client
--    SHOULD use the default system or application trust anchors,
--    unless otherwise configured.
--
--     onServerCertificate: validateDefault
--
-- 2. The client MUST verify that the certificate contains the IP
--    address of the designating Unencrypted DNS Resolver in an
--    iPAddress entry of the subjectAltName extension as described in
--    Section 4.2.1.6 of [RFC5280].
--
--     ractionServerAltName = Just ip
--     onServerCertificate: + makeOnServerCertificate ractionServerAltName
--
-- Sec 6.3 says:
--
-- When performing discovery using resolver IP addresses, clients MUST
-- use the original IP address of the Unencrypted DNS Resolver as the
-- URI host for DoH requests.
--
--    rinfoServerName = Just $ show ip
--    H2.defaultClientConfig{H2.authority = fromMaybe ipstr rinfoServerName}
--
-- Note that since IP addresses are not supported by default in the
-- TLS SNI, resolvers that support discovery using IP addresses will
-- need to be configured to present the appropriate TLS certificate
-- when no SNI is present for DoT, DoQ, and DoH.
--
--    clientUseServerNameIndication = False
modifyForDDR :: SVCBInfo -> SVCBInfo
modifyForDDR si@SVCBInfo{..} = si{svcbInfoResolveInfos = map modify svcbInfoResolveInfos}
  where
    ip = nameTagIP svcbInfoNameTag
    modify ri =
        ri
            { rinfoActions =
                (rinfoActions ri){ractionServerAltName = Just ip}
            , rinfoServerName = Just $ show ip
            }

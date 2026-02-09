{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Query.API (
    foldResponseIterative,
    foldResponseIterative',
    foldResponseCached,
    replyMessage,
) where

-- GHC packages

-- other packages

-- dnsext packages
import DNS.Do53.Client (QueryControls)
import qualified DNS.Log as Log
import DNS.SEC (TYPE (..))
import DNS.Types
import qualified DNS.Types as DNS

-- this package
import DNS.Iterative.Imports hiding (local)
import DNS.Iterative.Query.Class
import DNS.Iterative.Query.Helpers
import DNS.Iterative.Query.Local (takeLocalResult)
import DNS.Iterative.Query.Resolve
import DNS.Iterative.Query.Types
import DNS.Iterative.Query.Utils (logLn, pprMessage)

-----

-- | Folding a response corresponding to a query. The cache is maybe updated.
foldResponseIterative :: (String -> a) -> (VResult -> DNSMessage -> a) -> Env -> DNSMessage -> IO a
foldResponseIterative deny reply env@Env{..} reqM@DNSMessage{..} =
    foldResponse "resp-queried" deny reply env reqM (resolveStub reply nsid_ identifier question ednsHeader)

-- | Folding a response corresponding to a query, from questions and control flags. The cache is maybe updated.
foldResponseIterative'
    :: (String -> a) -> (VResult -> DNSMessage -> a) -> Env -> Identifier -> Question -> Question -> QueryControls -> IO a
foldResponseIterative' deny reply env@Env{..} ident qs q =
    queryControls' $ \fl eh -> foldResponse' "resp-queried'" deny reply env ident q fl eh (resolveStub reply nsid_ ident qs eh)

resolveStub :: MonadQuery m => (VResult -> DNSMessage -> a) -> Maybe OD_NSID -> Identifier -> Question -> EDNSheader -> m a
resolveStub reply nsid ident q eh = do
    ((cnrrs, _rn), etm) <- resolve =<< asksQP origQuestion_
    reqDO <- asksQP requestDO_
    let result rc vans vauth = replyMessage reqDO cnrrs rc vans vauth ident q eh nsid reply
    pure $ either (\(rc, vans, vauth) -> result rc vans vauth) (\(msg, vans, vauth) -> result (rcode msg) vans vauth) etm

-- | Folding a response corresponding to a query from the cache.
foldResponseCached :: DNSQuery a -> (String -> a) -> (VResult -> DNSMessage -> a) -> Env -> DNSMessage -> IO a
foldResponseCached misshit deny reply env@Env{..} reqM@DNSMessage{..} = foldResponse "resp-cached" deny reply env reqM $ do
    ((cnrrs, _rn), m) <- resolveByCache =<< asksQP origQuestion_
    reqDO <- asksQP requestDO_
    let hit (rc, vans, vauth) = replyMessage reqDO cnrrs rc vans vauth identifier question ednsHeader nsid_ reply
    maybe misshit (pure . hit) m

{- FOURMOLU_DISABLE -}
replyMessage
    :: RequestDO -> [RRset] -> RCODE -> [RRset] -> [RRset]
    -> Identifier -> Question -> EDNSheader -> Maybe OD_NSID -> (VResult -> DNSMessage -> a) -> a
replyMessage reqDO cnrrs rc vans vauth ident q eh nsid k = withResolvedRRs reqDO (cnrrs ++ vans) vauth withDO
  where
    withDO vres fs ans auth = k vres $ filterWithDO reqDO (h fs) ans auth
    h fs ans auth = replyDNSMessage eh nsid ident q rc fs ans auth
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
foldResponse
    :: String -> (String -> a) -> (VResult -> DNSMessage -> a)
    -> Env -> DNSMessage -> DNSQuery a -> IO a
foldResponse name deny reply env@Env{..} reqM@DNSMessage{question=q0@(Question bn typ cls),identifier=ident,flags=reqF,ednsHeader=reqEH} qaction =
    handleRequest env prefix reqM (pure . deny) ereply  result
  where
    ereply rc = pure $ reply VR_Insecure $ replyDNSMessage reqEH nsid_ ident q0 rc resFlags [] []
    result q = foldResponse' name deny reply env ident q reqF reqEH qaction
    prefix = name ++ ": orig-query " ++ show bn ++ " " ++ show typ ++ " " ++ show cls ++ ": "
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
foldResponse'
    :: String -> (String -> a) -> (VResult -> DNSMessage -> a)
    -> Env -> Identifier -> Question -> DNSFlags -> EDNSheader -> DNSQuery a -> IO a
foldResponse' name deny reply env@Env{..} ident q@(Question bn typ cls) reqF reqEH qaction  =
    takeLocalResult env q (pure $ deny "local-zone: query-denied") query (pure . local)
  where
    query = either eresult pure =<< runDNSQuery (logQueryErrors prefix qaction) env qparam
    eresult = queryErrorReply reqEH nsid_ ident q (pure . deny) ereplace
    {- replace response-code only when query, not replace for request-error or local-result -}
    ereplace vr resM = replaceRCODE env "query-error" (rcode resM) <&> \rc1 -> reply vr resM{rcode = rc1}
    local (rc, vans, vauth) = withResolvedRRs (requestDO_ qparam) vans vauth h
      where h vres fs ans = reply vres . replyDNSMessage reqEH nsid_ ident q rc fs ans
    qparam = queryParamH q reqF reqEH
    prefix = name ++ ": orig-query " ++ show bn ++ " " ++ show typ ++ " " ++ show cls ++ ": "
{- FOURMOLU_ENABLE -}

-----

{- FOURMOLU_DISABLE -}
logQueryErrors :: MonadQuery m => String -> m a -> m a
logQueryErrors prefix q = do
      q `catchQuery` left
    where
      left qe = do
          logQueryError qe
          throwQuery qe
      logQueryError qe = case qe of
          DnsError de ss           -> logDnsError de ss
          ExtraError ee addrs msg  -> extraError logNotResponse logInvalidEDNS logRcodeError logBogus ee addrs msg
      logDnsError de ss = case de of
          NetworkFailure {}   -> putLog detail
          DecodeError {}      -> putLog detail
          RetryLimitExceeded  -> putLog detail
          UnknownDNSError {}  -> putLog detail
          _                   -> pure ()
        where detail = show de ++ ": " ++ intercalate ", " ss
      logNotResponse  addrs msg  = putLog $ pprAddrs addrs ++ ":\n" ++ "not response" ++ maybe "" (pprMessage ":") msg
      logInvalidEDNS  DNS.InvalidEDNS addrs  msg = putLog $ pprAddrs addrs ++ ":\n" ++ "invalid EDNS" ++ maybe "" (pprMessage ":") msg
      logInvalidEDNS  _               _     _msg = pure ()
      logRcodeError _rcode _addrs _msg = pure ()
      logBogus _es _addrs _msg = pure ()
      pprAddrs = unwords . map show
      putLog = logLn Log.WARN . (prefix ++)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
handleRequest :: Env -> String -> DNSMessage -> (String -> IO a) -> (RCODE -> IO a) -> (Question -> IO a) -> IO a
handleRequest env prefix DNSMessage{flags = reqF,ednsHeader=reqEH,question=q} _deny ereply h
    | reqEH == DNS.InvalidEDNS   = ereply' DNS.ServFail   "InvalidEDNS"
    | not (DNS.recDesired reqF)  = ereply' DNS.Refused    "RD flag required"
    | otherwise                  = h q
  where
    ereply' rc s = elog s >> ereply rc
    elog s = logLines_ env Log.INFO Nothing ["request error: " ++ prefix ++ s]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
replaceRCODE :: Env -> String -> RCODE -> IO RCODE
replaceRCODE env tag rc0 = unless (rc0 == rc1) putLog $> rc1
  where
    putLog = logLines_ env Log.INFO Nothing [tag ++ ": replace response-code for query: " ++ show rc0 ++ " -> " ++ show rc1]
    rc1 = case rc0 of
        DNS.Refused  ->  DNS.ServFail
        x            ->  x
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
queryErrorReply
    :: EDNSheader -> Maybe OD_NSID
    -> Identifier -> Question
    -> (String -> a) -> (VResult -> DNSMessage -> a) -> QueryError -> a
queryErrorReply reqEH nsid ident rq left right qe = case qe of
    DnsError e _       -> dnsError e
    ExtraError ee _ _  -> extraError (insec srvFail) (\_ -> insec srvFail) (insec . message) (\_ -> right VR_Bogus srvFail) ee
  where
    dnsError e = foldDNSErrorToRCODE (left $ "DNSError: " ++ show e) (insec . message) e
    insec = right VR_Insecure
    srvFail = message ServFail
    message rc = replyDNSMessage reqEH nsid ident rq rc resFlags [] []
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
replyDNSMessage
    :: EDNSheader -> Maybe OD_NSID
    -> Identifier -> Question -> RCODE -> DNSFlags -> [RR] -> [RR] -> DNSMessage
replyDNSMessage reqEH nsid ident rq rcode flags rrs auth =
    ednsHeaderCases (withRequestEDNS nsid res) res res reqEH
  where
    res = DNS.defaultResponse
          { DNS.identifier = ident
          , DNS.rcode = rcode
          , DNS.flags = flags
          , DNS.answer = rrs
          , DNS.authority = auth
          , DNS.question = rq
          }

withRequestEDNS :: Maybe OD_NSID -> DNSMessage -> EDNS -> DNSMessage
withRequestEDNS nsid' res0 reqEDNS = addNSID nsid' res0
  where
    addNSID  Nothing    res  = res
    addNSID (Just nsid) res  = ednsHeaderCases has no res (ednsHeader res)
      where
        fill = mapOData (\(OD_NSID{}) -> OData nsid) (ednsOptions reqEDNS)
        has edns  = res{ednsHeader = EDNSheader edns{ednsOptions = fill ++ ednsOptions edns}}
        no        = res{ednsHeader = EDNSheader defaultEDNS{ednsOptions = fill}}
{- FOURMOLU_ENABLE -}

filterWithDO :: RequestDO -> ([RR] -> [RR] -> a) -> ([RR] -> [RR] -> a)
filterWithDO reqDO k2 ans auth =
    k2 (denyAnswer reqDO ans) (allowAuthority reqDO auth)
  where
    denyAnswer DnssecOK rrs = rrs
    denyAnswer NoDnssecOK rrs = foldr takeNODNSSEC [] rrs
      where
        takeNODNSSEC rr@ResourceRecord{..} xs
            | rrtype `elem` dnssecTypes = xs
            | otherwise = rr : xs

    allowAuthority NoDnssecOK = foldr takeSOA []
      where
        takeSOA rr@ResourceRecord{rrtype = SOA} xs = rr : xs
        takeSOA _ xs = xs
    allowAuthority DnssecOK = foldr takeAuth []
      where
        allowTypes = SOA : dnssecTypes
        takeAuth rr@ResourceRecord{..} xs
            | rrtype `elem` allowTypes = rr : xs
            | otherwise = xs

    dnssecTypes = [DNSKEY, DS, RRSIG, NSEC, NSEC3]

{- FOURMOLU_DISABLE -}
withResolvedRRs :: RequestDO -> [RRset] -> [RRset] -> (VResult -> DNSFlags -> [RR] -> [RR] -> a) -> a
withResolvedRRs reqDO ans auth h = h vres resFlags{authenData = allValid} (fromRRsets ans) (fromRRsets auth)
  where
    fromRRsets = concatMap $ rrListFromRRset reqDO
    vres = if allValid then VR_Secure else VR_Insecure
    allValid = not (null rrsets) && all rrsetValid rrsets
    rrsets = ans ++ auth
{- FOURMOLU_ENABLE -}

rrListFromRRset :: RequestDO -> RRset -> [ResourceRecord]
rrListFromRRset reqDO rs@RRset{..} = case reqDO of
    NoDnssecOK -> rrs
    DnssecOK -> case rrsRDatas of
        [] -> []
        _ : _ -> rrs ++ sigs
  where
    rrs =
        [ ResourceRecord rrsName rrsType rrsClass rrsTTL rd
        | rd <- rrsRDatas
        ]
    sigs =
        [ ResourceRecord rrsName RRSIG rrsClass rrsTTL (DNS.toRData sig)
        | sig <- rrsetGoodSigs rs
        ]

{- FOURMOLU_DISABLE -}
resFlags :: DNSFlags
resFlags =
    DNSFlags
    { isResponse    = True
    , authAnswer    = False
    , trunCation    = False
    , recDesired    = False
    , recAvailable  = True
    , authenData    = False
    , chkDisable    = False
    }
{- FOURMOLU_ENABLE -}

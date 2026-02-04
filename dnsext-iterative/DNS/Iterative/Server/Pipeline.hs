{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.Iterative.Server.Pipeline (
    mkPipeline,
    mkConnector,
    mkConnector',
    mkInput,
    noPendingOp,
    pendingOp,
    getWorkerStats,
    VcFinished (..),
    VcPendings,
    VcTimer (..),
    VcSession (..),
    withVcTimer,
    initVcSession,
    initVcSession',
    waitVcInput,
    waitVcOutput,
    enableVcEof,
    enableVcTimeout,
    addVcPending,
    delVcPending,
    showVcPendings,
    nullVcPendings,
    dequeueVcPendings,
    checkReceived,
    controlledRecvVC,
    receiverVC,
    receiverVCnonBlocking,
    getSendVC,
    senderVC,
    senderLogic,
    receiverLogic,
    receiverLogic',
    logLn,
    retryUntil,
    exceptionCase,
) where

-- GHC packages
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException (..), SomeException (..), bracket, handle, throwIO, try)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.IntSet as Set
import GHC.Event (TimeoutKey, TimerManager, getSystemTimerManager, registerTimeout, unregisterTimeout, updateTimeout)

-- libs

-- dnsext packages
import DNS.Do53.Internal (VCLimit, decodeVCLength)
import qualified DNS.Log as Log
import DNS.TAP.Schema (HttpProtocol (..), SocketProtocol (DOH, DOQ, DOT))
import qualified DNS.TAP.Schema as DNSTAP
import qualified DNS.ThreadStats as TStat
import DNS.Types
import qualified DNS.Types.Decode as DNS
import qualified DNS.Types.Encode as DNS
import DNS.Types.Time

-- this package
import DNS.Iterative.Imports
import DNS.Iterative.Internal (Env (..))
import DNS.Iterative.Query (VResult (..), foldResponseCached, foldResponseIterative)
import DNS.Iterative.Server.CtlRecv
import DNS.Iterative.Server.Types
import DNS.Iterative.Server.WorkerStats hiding (setWorkerStat)
import qualified DNS.Iterative.Server.WorkerStats as WStat
import DNS.Iterative.Stats

----------------------------------------------------------------

getWorkerStats :: Int -> IO [WorkerStatOP]
getWorkerStats workersN = replicateM workersN getWorkerStatOP

----------------------------------------------------------------

-- |
-- @
--                          |------ Pipeline ------|
--
--                                       Iterative IO
--                                         Req Resp
--                  ToCacher  cache         ^   |
--                              |           |   v
--        +--------+ shared +--------+    +--------+    +--------+
-- Req -> | recver | -----> | cacher | -> | worker | -> | sender | -> Resp
--        +--------+ or any +--------|    +--------+    +--------+
--                               |                          ^
--                               +--------------------------+
--                                        Cache hit
--                 Input BS       Input Msg        Output
-- @
mkPipeline
    :: Env
    -> [WorkerStatOP]
    -> [WorkerStatOP]
    -> IO ([IO ()], [IO ()], ToCacher -> IO ())
    -- ^ (worker actions, cacher actions, input to cacher)
mkPipeline env cacherStats workerStats = do
    {- limit waiting area on server to constant size -}
    let queueBound = 64
    qr <- newTBQueueIO queueBound
    let toCacher = atomically . writeTBQueue qr
        fromReceiver = atomically $ readTBQueue qr
    qw <- newTBQueueIO queueBound
    let toWorker = atomically . writeTBQueue qw
        fromCacher = atomically $ readTBQueue qw
    let cachers = [cacherLogic env cstat fromReceiver toWorker | cstat <- cacherStats]
    let workers = [workerLogic env wstat fromCacher | wstat <- workerStats]
    return (cachers, workers, toCacher)

----------------------------------------------------------------

data CacheResult
    = CResultMissHit
    | CResultHit VResult DNSMessage
    | CResultDenied String

inputAddr :: Input a -> String
inputAddr Input{..} = show inputPeerInfo ++ " -> " ++ show inputMysa

cacherLogic :: Env -> WorkerStatOP -> IO FromReceiver -> (ToWorker -> IO ()) -> IO ()
cacherLogic env wstat fromReceiver toWorker = handledLoop env "cacher" $ do
    let setWorkerStat = setWorkerStatEV wstat
    setWorkerStat WWaitDequeue
    inpBS@Input{..} <- fromReceiver
    case DNS.decode inputQuery of
        Left e -> logLn env Log.WARN $ "cacher.decode-error: " ++ inputAddr inpBS ++ " : " ++ show e
        Right queryMsg -> do
            -- Input ByteString -> Input DNSMessage
            let qs = question queryMsg
            setWorkerStat (WRun qs)
            let inp = inpBS{inputQuery = queryMsg}
            cres <- foldResponseCached (pure CResultMissHit) CResultDenied CResultHit env queryMsg
            setWorkerStat $ WWaitEnqueue qs inputDoX EnBegin
            case cres of
                CResultMissHit -> do
                    setWorkerStat $ WWaitEnqueue qs inputDoX (EnCCase "CResultMissHit")
                    toWorker inp
                CResultHit vr replyMsg -> do
                    setWorkerStat $ WWaitEnqueue qs inputDoX (EnCCase $ "CResultHit" ++ show vr)
                    duration <- diffUsec <$> currentTimeUsec_ env <*> pure inputRecvTime
                    updateHistogram_ env duration (stats_ env)
                    mapM_ (incStats $ stats_ env) [statsIxOfVR vr, CacheHit, QueriesAll]
                    let bs = encodeWithTC env inputPeerInfo (ednsHeader queryMsg) replyMsg
                    setWorkerStat $ WWaitEnqueue qs inputDoX EnTap
                    record env inp replyMsg bs
                    setWorkerStat $ WWaitEnqueue qs inputDoX EnSend
                    inputToSender $ Output bs inputPendingOp inputPeerInfo
                CResultDenied _replyErr -> do
                    setWorkerStat $ WWaitEnqueue qs inputDoX (EnCCase "CResultDenied")
                    duration <- diffUsec <$> currentTimeUsec_ env <*> pure inputRecvTime
                    updateHistogram_ env duration (stats_ env)
                    logicDenied env inp
                    vpDelete inputPendingOp
            setWorkerStat $ WWaitEnqueue qs inputDoX EnEnd

----------------------------------------------------------------

workerLogic :: Env -> WorkerStatOP -> IO FromCacher -> IO ()
workerLogic env wstat fromCacher = handledLoop env "worker" $ do
    let setWorkerStat = setWorkerStatEV wstat
    setWorkerStat WWaitDequeue
    inp@Input{..} <- fromCacher
    let qs = question inputQuery
    setWorkerStat (WRun qs)
    ex <- foldResponseIterative Left (curry Right) env inputQuery
    duration <- diffUsec <$> currentTimeUsec_ env <*> pure inputRecvTime
    updateHistogram_ env duration (stats_ env)
    setWorkerStat $ WWaitEnqueue qs inputDoX EnBegin
    case ex of
        Right (vr, replyMsg) -> do
            mapM_ (incStats $ stats_ env) [statsIxOfVR vr, CacheMiss, QueriesAll]
            let bs = encodeWithTC env inputPeerInfo (ednsHeader inputQuery) replyMsg
            setWorkerStat $ WWaitEnqueue qs inputDoX EnTap
            record env inp replyMsg bs
            setWorkerStat $ WWaitEnqueue qs inputDoX EnSend
            inputToSender $ Output bs inputPendingOp inputPeerInfo
        Left _e -> logicDenied env inp
    setWorkerStat $ WWaitEnqueue qs inputDoX EnEnd

----------------------------------------------------------------

setWorkerStatEV :: WorkerStatOP -> WorkerStat -> IO ()
setWorkerStatEV wstat st = do
    WStat.setWorkerStat wstat st
    TStat.eventLog $ "iter.st " ++ show st

----------------------------------------------------------------

encodeWithTC :: Env -> Peer -> EDNSheader -> DNSMessage -> BS
encodeWithTC env peer reqEH res = handleUdpLimit (udpLimit_ env) reqEH res (handleTC peer $ \_ bs -> bs)

{- FOURMOLU_DISABLE -}
{- https://datatracker.ietf.org/doc/html/rfc2181#section-9
   The TC (truncated) header bit
     "
      The TC bit should not be set merely because some extra information
      could have been included, but there was insufficient room.  This
      includes the results of additional section processing.  In such cases
      the entire RRSet that will not fit in the response should be omitted,
      and the reply sent as is, with the TC bit clear.  If the recipient of
      the reply needs the omitted data, it can construct a query for that
      data and send that separately.
     " -}
handleTC :: Peer -> (DNSMessage -> BS -> a) -> Word16 -> DNSMessage -> a
handleTC (PeerInfoUDP {}) h lim' r0
    | lim < BS.length bs1            = h tc (DNS.encode tc)  {- case: lim < len r1                               -}
    | BS.length bs0 < lim            = h r0 bs0              {- case:                              len r0 <= lim -}
    | otherwise                      = h r1 bs1              {- case:       len r1 <= lim && lim < len r0        -}
  where
    ~bs0 = DNS.encode r0
    ~bs1 = DNS.encode r1
    ~r1 = r0{additional = []}
    ~tc = r0{flags = (flags r0){trunCation = True}, answer = [], authority = [], additional = []}
    lim = fromIntegral lim'
handleTC _peer            h _lim r0  = h r0 (DNS.encode r0)
{- FOURMOLU_ENABLE -}

handleUdpLimit :: Word16 -> EDNSheader -> DNSMessage -> (Word16 -> DNSMessage -> a) -> a
handleUdpLimit srvLimit reqEH res h = h udpLimit res{ednsHeader = respEH}
  where
    respEH = ednsHeaderCases setRespUdp NoEDNS InvalidEDNS (ednsHeader res)
    setRespUdp redns = EDNSheader redns{ednsUdpSize = udpLimit}
    udpLimit = ednsHeaderCases limitEDNS 512 512 reqEH
    -- apply the smaller of client-request and server-conf
    limitEDNS qedns = (minUdpSize `max` (ednsUdpSize qedns `min` maxUdpSize)) `min` srvLimit

----------------------------------------------------------------

logicDenied :: Env -> Input DNSMessage -> IO ()
logicDenied env _inp@Input{} = do
    mapM_ (incStats $ stats_ env) [ResolveDenied, QueriesAll]

{- {- not reply for deny case. -}
let replyMsg =
        inputQuery
            { flags = (flags inputQuery){isResponse = True}
            , rcode = FormatErr
            }
let bs = DNS.encode replyMsg
record env inp replyMsg bs
inputToSender $ Output bs inputPeerInfo
 -}

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
statsIxOfVR :: VResult -> StatsIx
statsIxOfVR VR_Secure    = VResSecure
statsIxOfVR VR_Insecure  = VResInsecure
statsIxOfVR VR_Bogus     = VResBogus
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
applyProtoDNSTAP :: DoX -> (SocketProtocol -> HttpProtocol -> a) -> a
applyProtoDNSTAP UDP h = h DNSTAP.UDP HTTP_NONE
applyProtoDNSTAP TCP h = h DNSTAP.TCP HTTP_NONE
applyProtoDNSTAP DoT h = h        DOT HTTP_NONE
applyProtoDNSTAP H2  h = h        DOH HTTP2
applyProtoDNSTAP H2C h = h        DOH HTTP2
applyProtoDNSTAP H3  h = h        DOH HTTP3
applyProtoDNSTAP DoQ h = h        DOQ HTTP_NONE
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
record
    :: Env
    -> Input DNSMessage
    -> DNSMessage
    -> ByteString
    -> IO ()
record env Input{..} reply rspWire = do
    let peersa = peerSockAddr inputPeerInfo
    logDNSTAP_ env $ runEpochTimeUsec inputRecvTime $
        \s us -> applyProtoDNSTAP inputDoX $ \proto httpProto ->
            DNSTAP.composeMessage proto inputMysa peersa s (fromIntegral us * 1000) rspWire httpProto
    let st = stats_ env
        Question{..} = question inputQuery
        DNSFlags{..} = flags reply
    case ednsHeader inputQuery of
        EDNSheader (EDNS{..})
            | ednsDnssecOk -> incStats st QueryDO
        _ -> pure ()
    incStatsM st fromQueryTypes qtype (Just QueryTypeOther)
    incStatsM st fromDNSClass qclass (Just DNSClassOther)
    let rc = rcode reply
    incStatsM st fromRcode rc Nothing
    when (rc == NoErr) $
        if null (answer reply)
            then incStats st RcodeNoData
            else incStats st RcodeNoError
    when authAnswer   $ incStats st FlagAA
    when authenData   $ incStats st FlagAD
    when chkDisable   $ incStats st FlagCD
    when isResponse   $ incStats st FlagQR
    when recAvailable $ incStats st FlagRA
    when recDesired   $ incStats st FlagRD
    when trunCation   $ incStats st FlagTC
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

type BS = ByteString

type MkInput = ByteString -> Peer -> VcPendingOp -> EpochTimeUsec -> Input ByteString

{- FOURMOLU_DISABLE -}
mkInput :: SockAddr -> (ToSender -> IO ()) -> DoX -> MkInput
mkInput mysa toSender dox bs peerInfo pendingOp' ts =
    Input
    { inputQuery      = bs
    , inputPendingOp  = pendingOp'
    , inputMysa       = mysa
    , inputPeerInfo   = peerInfo
    , inputDoX        = dox
    , inputToSender   = toSender
    , inputRecvTime   = ts
    }
{- FOURMOLU_ENABLE -}

checkReceived :: Int -> VcTimer -> ByteString -> IO ()
checkReceived slsize timer bs = do
    let sz = BS.length bs
    when (sz > slsize || sz == 0) $ resetVcTimer timer

receiverVC
    :: String
    -> Env
    -> VcSession
    -> IO (BS, Peer)
    -> (ToCacher -> IO ())
    -> MkInput
    -> IO VcFinished
receiverVC name env vcs@VcSession{..} recv toCacher mkInput_ =
    loop 1 `E.catch` onError
  where
    -- SomeException: asynchronous exceptions are re-thrown
    onError se@(SomeException e) = warnOnError env name se >> throwIO e
    loop i = cases =<< waitVcInput vcs
      where
        cases timeout
            | timeout = pure VfTimeout
            | otherwise = do
                (bs, peerInfo) <- recv
                ts <- currentTimeUsec_ env
                casesSize bs peerInfo ts
        casesSize bs peerInfo ts
            | BS.null bs = caseEof
            | otherwise = step bs peerInfo ts >> loop (i + 1)

        caseEof = atomically (enableVcEof vcEof_) >> return VfEof
        step bs peerInfo ts = pendingOp vcPendings_ i $ \add pop -> do
            add
            toCacher $ mkInput_ bs peerInfo pop ts

-- Since repeating event waiting and non-blocking reads,
-- `controlledRecvVC` itself blocks on event waiting.
controlledRecvVC
    :: CtlRecv
    -> (Int -> IO BS)
    -- ^ Receiving function
    -> VCLimit
    -- ^ VC length limit
    -> IO (Either Terminate BS)
controlledRecvVC ctl recvN lim = go
  where
    go = do
        en <- withControlledRecv ctl recvN 2 $ \bs ->
            return $ decodeVCLength bs
        case en of
            Left term -> return (Left term)
            Right len
                | fromIntegral len > lim ->
                    E.throwIO $
                        DecodeError $
                            "length is over the limit: should be len <= lim, but (len: "
                                ++ show len
                                ++ ") > (lim: "
                                ++ show lim
                                ++ ")"
                | otherwise -> withControlledRecv ctl recvN len return

receiverVCnonBlocking
    :: String
    -> Env
    -> VCLimit
    -> VcSession
    -> Peer
    -> (Int -> IO BS)
    -> (BS -> IO ())
    -> (ToCacher -> IO ())
    -> MkInput
    -> IO VcFinished
receiverVCnonBlocking name env lim vcs@VcSession{..} peerInfo recvN onRecv toCacher mkInput_ = do
    ctl <- newCtlRecv $ waitVcInput vcs
    loop ctl 1 `E.catch` onError
  where
    -- SomeException: asynchronous exceptions are re-thrown
    onError se@(SomeException e) = warnOnError env name se >> throwIO e
    loop ctl i = do
        ex <- controlledRecvVC ctl recvN lim
        case ex of
            Left EOF -> caseEof
            Left Break -> return VfTimeout
            Right bs -> do
                onRecv bs
                ts <- currentTimeUsec_ env
                step bs ts
                loop ctl (i + 1)
      where
        caseEof = atomically (enableVcEof vcEof_) >> return VfEof
        step bs ts = pendingOp vcPendings_ i $ \add pop -> do
            add
            toCacher $ mkInput_ bs peerInfo pop ts

receiverLogic
    :: Env -> SockAddr -> IO (BS, Peer) -> (ToCacher -> IO ()) -> (ToSender -> IO ()) -> DoX -> IO ()
receiverLogic env mysa recv toCacher toSender dox =
    handledLoop env "receiverUDP" $ void $ receiverLogic' env mysa recv toCacher toSender dox

receiverLogic'
    :: Env -> SockAddr -> IO (BS, Peer) -> (ToCacher -> IO ()) -> (ToSender -> IO ()) -> DoX -> IO Bool
receiverLogic' env mysa recv toCacher toSender dox = do
    (bs, peerInfo) <- recv
    ts <- currentTimeUsec_ env
    if bs == ""
        then return False
        else do
            toCacher $ mkInput mysa toSender dox bs peerInfo noPendingOp ts
            return True

noPendingOp :: VcPendingOp
noPendingOp = VcPendingOp{vpReqNum = 0, vpDelete = pure ()}

pendingOp :: TVar VcPendings -> Int -> (IO () -> VcPendingOp -> a) -> a
pendingOp pendings i h = h addPending pop
  where
    addPending = atomically $ addVcPending pendings i
    delPending = atomically $ delVcPending pendings i
    pop = VcPendingOp{vpReqNum = i, vpDelete = delPending}

getSendVC :: VcTimer -> (BS -> Peer -> IO ()) -> BS -> Peer -> IO ()
getSendVC timer send bs peerInfo = resetVcTimer timer >> send bs peerInfo

senderVC
    :: String
    -> Env
    -> VcSession
    -> (BS -> Peer -> IO ())
    -> IO FromX
    -> IO VcFinished
senderVC name env vcs send fromX = loop `E.catch` onError
  where
    -- logging async exception intentionally, for not expected `cancel`
    -- SomeException: asynchronous exceptions are re-thrown
    onError se@(SomeException e) = warnOnError env name se >> throwIO e
    loop = do
        mx <- waitVcOutput vcs
        case mx of
            Just x -> return x
            Nothing -> step >> loop
    step = E.bracket fromX finalize $ \(Output bs _ peerInfo) -> send bs peerInfo
    finalize (Output _ VcPendingOp{..} _) = vpDelete

senderLogic :: Env -> (BS -> Peer -> IO ()) -> IO FromX -> IO ()
senderLogic env send fromX =
    handledLoop env "senderUDP" $ senderLogic' send fromX

senderLogic' :: (BS -> Peer -> IO ()) -> IO FromX -> IO ()
senderLogic' send fromX = do
    Output bs _ peerInfo <- fromX
    send bs peerInfo

----------------------------------------------------------------

type VcEof = Bool
type VcTimeout = Bool
type VcWaitRead = ()
type VcPendings = Set.IntSet
type VcRespAvail = Bool
type VcAllowInput = Bool

{- FOURMOLU_DISABLE -}
data VcTimer =
    VcTimer
    { vtManager_        :: TimerManager
    , vtKey_            :: TimeoutKey
    , vtMicrosec_       :: Int
    }
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- separate the STM computations that determine event states from
-- the conditions and parameters that cause these events to occur.
-- place only what is directly necessary for STM calculations in `VcSession`.
data VcSession =
    VcSession
    { vcEof_            :: TVar VcEof
    -- ^ EOF is received. This is an RX evet.
    , vcTimeout_        :: TVar VcTimeout
    -- ^ TimerManager tells timed-out. This is another RX event
    --   but defined independently on EOF to make recording event simpler.
    , vcPendings_       :: TVar VcPendings
    -- ^ A set of jobs. A job is that a request is received but
    --   a response is not sent. This design can take the pipeline
    --   as a blackbox since the sender increases it and the receiver
    --   decreases it.
    , vcRespAvail_      :: STM VcRespAvail
    -- ^ Jobs are available to the sender. This is necessary to
    --   tell whether or not the queue to the sender is empty or not
    --   WITHOUT IO.
    , vcAllowInput_     :: STM VcAllowInput
    , vcWaitRead_       :: IO (STM VcWaitRead)
    }
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
data VcFinished
    = VfEof
    | VfTimeout
    deriving (Eq, Show)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
initVcTimer :: Int -> IO () -> IO VcTimer
initVcTimer micro actionTO = do
    mgr <- getSystemTimerManager
    key <- registerTimeout mgr micro actionTO
    pure $ VcTimer mgr key micro
{- FOURMOLU_ENABLE -}

finalizeVcTimer :: VcTimer -> IO ()
finalizeVcTimer VcTimer{..} = unregisterTimeout vtManager_ vtKey_

withVcTimer
    :: Int
    -> IO ()
    -> (VcTimer -> IO a)
    -> IO a
withVcTimer micro actionTO = bracket (initVcTimer micro actionTO) finalizeVcTimer

initVcSession
    :: IO (STM VcWaitRead)
    -> IO (VcSession, ToSender -> IO (), IO FromX)
initVcSession getWaitIn = do
    (a, b, c, _) <- initVcSession' getWaitIn
    pure (a, b, c)

{- FOURMOLU_DISABLE -}
initVcSession'
    :: IO (STM VcWaitRead)
    -> IO (VcSession, ToSender -> IO (), IO FromX, TBQueue ToSender)
initVcSession' getWaitIn = do
    vcEof       <- newTVarIO False
    vcTimeout   <- newTVarIO False
    vcPendings  <- newTVarIO Set.empty
    let queueBound = 8 {- limit waiting area per session to constant size -}
    senderQ     <- newTBQueueIO queueBound
    let toSender = atomically . writeTBQueue senderQ
        fromX = atomically $ readTBQueue senderQ
        inputThreshold = succ queueBound `quot` 2
        {- allow room for cacher loops and worker loops to write -}
        allowInput = (<= inputThreshold) <$> lengthTBQueue senderQ
        result =
            VcSession
            { vcEof_            = vcEof
            , vcTimeout_        = vcTimeout
            , vcPendings_       = vcPendings
            , vcRespAvail_      = not <$> isEmptyTBQueue senderQ
            , vcAllowInput_     = allowInput
            , vcWaitRead_       = getWaitIn
            }
    pure (result, toSender, fromX, senderQ)
{- FOURMOLU_ENABLE -}

enableVcEof :: TVar VcEof -> STM ()
enableVcEof eof = writeTVar eof True

enableVcTimeout :: TVar VcTimeout -> STM ()
enableVcTimeout timeout = writeTVar timeout True

addVcPending :: TVar VcPendings -> Int -> STM ()
addVcPending pendings i = modifyTVar' pendings (Set.insert i)

delVcPending :: TVar VcPendings -> Int -> STM ()
delVcPending pendings i = modifyTVar' pendings (Set.delete i)

showVcPendings :: TVar VcPendings -> IO String
showVcPendings pendings = show <$> atomically (readTVar pendings)

nullVcPendings :: TVar VcPendings -> IO Bool
nullVcPendings pendings = Set.null <$> atomically (readTVar pendings)

resetVcTimer :: VcTimer -> IO ()
resetVcTimer VcTimer{..} = updateTimeout vtManager_ vtKey_ vtMicrosec_

dequeueVcPendings :: TVar VcPendings -> TBQueue ToSender -> IO ()
dequeueVcPendings pendings senderQ = loop
  where
    loop = do
        nn <- not . Set.null <$> atomically (readTVar pendings)
        when nn $ do
            Output _ VcPendingOp{..} _ <- atomically (readTBQueue senderQ)
            vpDelete
            when nn loop

waitVcInput :: VcSession -> IO Bool
waitVcInput VcSession{..} = do
    waitIn <- vcWaitRead_
    atomically $ do
        timeout <- readTVar vcTimeout_
        unless timeout $ do
            retryUntil =<< vcAllowInput_
            waitIn
        return timeout

{- FOURMOLU_DISABLE -}
--   eof       timeout   pending     avail       sender-loop
--
--   eof       to        null        no-avail    break
--   not-eof   to        null        no-avail    break
--   eof       not-to    null        no-avail    break
--   not-eof   not-to    null        no-avail    wait
--   -         -         not-null    no-avail    wait
--   -         -         -           avail       loop
--
-- If we consider to merge eof and timeout to rx state including
-- open|closed|timed-out, the table could be:
--
--   state     pending     avail       sender-loop
--
--   open      null        no-avail    wait
--   _         null        no-avail    break
--   -         not-null    no-avail    wait
--   -         -           avail       loop
waitVcOutput :: VcSession -> IO (Maybe VcFinished)
waitVcOutput VcSession{..} = atomically $ do
    mayEof <- toMaybe VfEof     <$> readTVar vcEof_
    mayTo  <- toMaybe VfTimeout <$> readTVar vcTimeout_
    avail  <- vcRespAvail_
    case mayEof <|> mayTo of
        -- Rx is open. Waiting for jobs for the sender without IO.
        -- When a job is available, Nothing is returned.
        Nothing -> retryUntil avail >> return Nothing
        -- Rx is closed.
        -- If jobs are available, just returns Nothing.
        -- Otherwise, the pipeline are processing jobs which
        -- are eventually passed to the sender if we "retry".
        -- After several retries AND available is false
        -- AND pending is null, the sender can finish.
        Just fc
            | avail -> return Nothing
            | otherwise -> do
                retryUntil . Set.null =<< readTVar vcPendings_
                return $ Just fc
  where
    toMaybe x True  = Just x
    toMaybe _ False = Nothing
{- FOURMOLU_ENABLE -}

retryUntil :: Bool -> STM ()
retryUntil = guard

-- retryUntil True  = pure () -- go to the next action
-- retryUntil False = retry   -- go to the beginning again

mkConnector :: IO (ToSender -> IO (), IO FromX, STM VcRespAvail, STM VcAllowInput)
mkConnector = do
    (a, b, c, d, _, _) <- mkConnector'
    pure (a, b, c, d)

mkConnector' :: IO (ToSender -> IO (), IO FromX, STM VcRespAvail, STM VcAllowInput, TBQueue ToSender, TVar VcPendings)
mkConnector' = do
    let queueBound = 8 {- limit waiting area per session to constant size -}
        inputThreshold = succ queueBound `quot` 2
    qs <- newTBQueueIO queueBound
    let toSender = atomically . writeTBQueue qs
        fromX = atomically $ readTBQueue qs
    vcPendings <- newTVarIO Set.empty
    return (toSender, fromX, not <$> isEmptyTBQueue qs, (<= inputThreshold) <$> lengthTBQueue qs, qs, vcPendings)

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
handledLoop :: Env -> String -> IO () -> IO ()
handledLoop env tag body = forever $ handle (\e -> loggingExp env Log.DEBUG tag e >> takeEx e) body
  where
    -- SomeException: asynchronous exceptions are re-thrown
    takeEx :: SomeException -> IO ()
    takeEx e
        | Just (SomeAsyncException _) <- E.fromException e  = throwIO e
        | otherwise                                         = pure ()
{- FOURMOLU_ENABLE -}

warnOnError :: Env -> String -> SomeException -> IO ()
warnOnError env tag e = loggingExp env Log.WARN tag e

loggingExp :: Env -> Log.Level -> String -> SomeException -> IO ()
loggingExp env lv tag (SomeException e) = logLn env lv (tag ++ ": exception: " ++ show e)

{- FOURMOLU_DISABLE -}
exceptionCase :: (String -> IO ()) -> IO a -> IO a
exceptionCase logLn' body = do
    e <- try body
    either handler pure e
  where
    logging e = logLn' $ "received exception: " ++ (show e)
    -- SomeException: asynchronous exceptions and others are re-thrown
    handler :: SomeException -> IO a
    handler e = logging e  >> throwIO e
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

logLn :: Env -> Log.Level -> String -> IO ()
logLn env level = logLines_ env level Nothing . (: [])

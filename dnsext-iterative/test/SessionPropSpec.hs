{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module SessionPropSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

--
import Control.Concurrent
import Control.Concurrent.Async (wait)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Functor
import Data.IORef
import Data.List hiding (insert)
import qualified Data.Map.Strict as Map
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--
import qualified DNS.ThreadAsync as TAsync

--
import DNS.Iterative.Server

------------------------------------------------------------

spec :: Spec
spec = prop "randomized events" prop_events

{- FOURMOLU_DISABLE -}
prop_events :: Property
prop_events =
   forAll chooseSize $ \size ->
       forAll (genSessionPattern size) $ \pat -> ioProperty $
           intervaledEvents interval (eventsFromPattern pat) <&>
           \result -> result === expectFromPattern pat
  where
    chooseSize =
       frequency
       [ (7, chooseInt (0, 5))
       , (2, chooseInt (6, 10))
       , (1, chooseInt (11, 15))
       ]
    interval = 16_000
{- FOURMOLU_ENABLE -}

------------------------------------------------------------
-- pseudo event definition for server session

type TaskNum = Int

{- FOURMOLU_DISABLE -}
data Event
  = EvRecv TaskNum
  | EvSend TaskNum
  | EvEof
  | EvTimeout
  deriving Eq
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
instance Show Event where
  show (EvRecv tn)  = "R" ++ show tn
  show (EvSend tn)  = "S" ++ show tn
  show  EvEof       = "Eof"
  show  EvTimeout   = "Timeout"
{- FOURMOLU_ENABLE -}

instance Read Event where
    readsPrec _ s = case s of
        'R' : ns -> applyReads EvRecv ns
        'S' : ns -> applyReads EvSend ns
        _ -> matchPrefix "Eof" EvEof ++ matchPrefix "Timeout" EvTimeout
      where
        applyReads f ns = [(f n, x) | (n, x) <- reads ns]
        matchPrefix pre x
            | hd == pre = [(x, tl)]
            | otherwise = []
          where
            (hd, tl) = splitAt (length pre) s

type MilliSec = Int
type BS = ByteString

------------------------------------------------------------

intervaledEvents :: Int -> [Event] -> IO ((VcFinished, VcFinished), [ByteString])
intervaledEvents micro evs = do
    debug <- maybe False ((== "1") . take 1) <$> lookupEnv "VCTEST_DEBUG"
    pushEvents debug (threadDelay micro) evs

interactiveEvents :: [Event] -> IO ((VcFinished, VcFinished), [ByteString])
interactiveEvents = pushEvents True (void getLine)

_size1ex :: [Event]
_size1ex = [EvRecv 1, EvSend 1, EvEof, EvTimeout]

_run1ex :: IO ((VcFinished, VcFinished), [ByteString])
_run1ex = interactiveEvents _size1ex

_size10ex :: [Event]
_size10ex = read "[R4,R8,R5,R9,R2,R10,S9,R7,S5,S7,R1,S4,R3,S1,Timeout,R6,Eof,S2,S6,S10,S8,S3]"

_run10ex :: IO ((VcFinished, VcFinished), [ByteString])
_run10ex = interactiveEvents _size10ex

------------------------------------------------------------
-- trigger events along with list order

pushEvents :: Bool -> IO () -> [Event] -> IO ((VcFinished, VcFinished), [ByteString])
pushEvents debug interval evs = do
    (push, dump, run, getResult) <- runWithEvent
    rh <- TAsync.async "test" run
    let iloop [] = pure ()
        iloop (e : es) = do
            interval
            when debug $ do
                dump
                putStrLn $ "event: " ++ show e
            push e
            iloop es
    iloop evs
    (,) <$> wait rh <*> getResult

runWithEvent :: IO (Event -> IO (), IO (), IO (VcFinished, VcFinished), IO [ByteString])
runWithEvent = do
    env <- newEmptyEnv
    (getResult0, send) <- getSend
    (toCacher, kickSender) <- pseudoPipeline readMaybe
    refWait <- newIORef (pure ())
    let myaddr = SockAddrInet 53 0x0100007f
    initVcSession retry (readIORef refWait) >>= \(vcSess@VcSession{..}, toSender, fromX) -> do
        let enableTimeout = atomically (enableVcTimeout vcTimeout_ {- enable timeout with pushed events -})
        (loop, pushEvent, waitRecv, recv) <- eventsRunner show kickSender enableTimeout
        writeIORef refWait waitRecv {- fill action to ref, to avoid mutual reference of withVcSession and eventsRunner -}
        let (_synth, foldCached, foldIterative) = extendSynthesis SynthNone
            receiver = receiverVC "test-recv" env vcSess recv toCacher (mkInput myaddr toSender UDP SynthNone foldCached foldIterative)
            sender = senderVC "test-send" env vcSess send fromX
            run = TAsync.concurrently "test-send" sender "test-recv" receiver
            getResult = sort <$> getResult0
        _ <- forkIO loop
        pure (pushEvent, dumpSession vcSess, run, getResult)

dumpSession :: VcSession -> IO ()
dumpSession VcSession{..} = do
    (e, p, a) <- atomically $ (,,) <$> readTVar vcEof_ <*> readTVar vcPendings_ <*> vcRespAvail_
    putStrLn $ unwords ["eof:", show e, "pendings:", show p, "avail:", show a]

getSend :: IO (IO [ByteString], BS -> Peer -> IO ())
getSend = do
    ref <- newIORef []
    pure (readIORef ref, \x _ -> sstep ref x)
  where
    ins x s = (x : s, ())
    sstep ref x = atomicModifyIORef' ref (ins x)

dummyPeer :: Peer
dummyPeer = PeerInfoVC $ SockAddrInet 12345 0x0100007f

------------------------------------------------------------
-- run issued events for pseudo pipeline

{- FOURMOLU_DISABLE -}
eventsRunner
    :: (TaskNum -> String) -> (TaskNum -> IO ()) -> IO ()
    -> IO (IO a, Event -> IO (), STM (), IO (BS, Peer))
eventsRunner showTaskNum kickSender enableTimeout = do
    evQ   <- newTQueueIO
    recvQ <- newTQueueIO
    let consumeEvent e = case e of
            EvRecv n   -> atomically $ writeTQueue recvQ $ B8.pack $ showTaskNum n
            EvEof      -> atomically $ writeTQueue recvQ $ B8.pack   ""
            EvSend n   -> kickSender n
            EvTimeout  -> enableTimeout
        eventLoop = forever $ consumeEvent =<< atomically (readTQueue evQ)
        pushEvent = atomically . writeTQueue evQ
        waitRecv = guard . not =<< isEmptyTQueue recvQ
        recv = (,) <$> atomically (readTQueue recvQ) <*> pure dummyPeer
    pure (eventLoop, pushEvent, waitRecv, recv)
{- FOURMOLU_ENABLE -}

pseudoPipeline :: (String -> Maybe TaskNum) -> IO (ToCacher -> IO (), TaskNum -> IO ())
pseudoPipeline readTaskNum = do
    tasksRef <- newTVarIO Map.empty
    mq <- newTQueueIO
    let inputLoop = forever $ atomically $ do
            Input{..} <- readTQueue mq
            let toSender = inputToSender $ Output inputQuery inputPendingOp inputPeerInfo
                run taskNum = modifyTVar' tasksRef $ Map.insert taskNum toSender
            maybe (pure ()) run $ readTaskNum $ B8.unpack inputQuery
        kickSender taskNum = do
            toSender <- atomically $ do
                let getAct tasks =
                        maybe (pure (), tasks) found $ Map.lookup taskNum tasks
                      where
                        found x = (x, Map.delete taskNum tasks)
                stateTVar tasksRef getAct
            toSender

    replicateM_ 4 (forkIO inputLoop)
    pure (atomically . writeTQueue mq, kickSender)

------------------------------------------------------------
-- generating randomized event list for session

{- FOURMOLU_DISABLE -}
splitTailSends :: [Event] -> ([Event], [Event])
splitTailSends es = (reverse r2, reverse r1)
  where
    (r1, r2) = span notRecv (reverse es)
    notRecv EvSend{} = True
    notRecv EvRecv{} = False
    notRecv _        = True
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
genSplitted :: [a] -> Gen ([a], [a])
genSplitted es
    | len <= threshold + 2  = chooseInt (0, length es) <&> (`splitAt` es)
    | otherwise             = chooseInt_               <&> (`splitAt` es)
  where
    threshold = 5
    len = length es
    chooseInt_ = frequency [(1, pure 0), (threshold, chooseInt (1, len - 1)), (1, pure len)]
{- FOURMOLU_ENABLE -}

newtype SessionPattern = SessionPattern (Either ([Event], [Event], [Event]) ([Event], [Event], [Event]))

sessionPattern :: (([Event], [Event], [Event]) -> a) -> (([Event], [Event], [Event]) -> a) -> SessionPattern -> a
sessionPattern caseTimeout caseEof (SessionPattern e) = either caseTimeout caseEof e

{- FOURMOLU_DISABLE -}
eventsFromPattern :: SessionPattern -> [Event]
eventsFromPattern = sessionPattern timeout eof
  where
    timeout (be1, be2, ae) = be1 ++ [EvTimeout] ++ be2 ++ [EvEof] ++ ae
    eof     (be, ae1, ae2) = be ++ [EvEof] ++ ae1 ++ [EvTimeout] ++ ae2
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
expectFromPattern :: SessionPattern -> ((VcFinished, VcFinished), [ByteString])
expectFromPattern = sessionPattern timeout eof
  where
    timeout (be1, _, _)  = ((VfTimeout, VfTimeout), sort [B8.pack $ show n | EvRecv n <- be1])
    eof     (be, _, _)   = ((VfEof,     VfEof),     sort [B8.pack $ show n | EvRecv n <- be])
{- FOURMOLU_ENABLE -}

instance Show SessionPattern where
    show = ("pattern-events: " ++) . show . eventsFromPattern

{- FOURMOLU_DISABLE -}
genSessionPattern :: Int -> Gen SessionPattern
genSessionPattern n = do
    es <- genRecvSendList n
    let (hd, ss) = splitTailSends es
    (ss1, ae) <- genSplitted ss
    let be = hd ++ ss1
        timeout = genSplitted be <&> \(be1, be2) -> Left (be1, be2, ae)
        eof =     genSplitted ae <&> \(ae1, ae2) -> Right (be, ae1, ae2)
    SessionPattern <$> oneof [timeout, eof]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
checkSessionEvents :: Int -> [Event] -> [Bool]
checkSessionEvents n evs =
    checkRecvSendList n evs ++ [lastEv == EvEof]
  where
    lastEv = last [ev' | ev <- evs, ev' <- recvEv ev]
    recvEv ev@(EvRecv {})  = [ev]
    recvEv ev@EvEof        = [ev]
    recvEv     _           = []
{- FOURMOLU_ENABLE -}

_checkEventGenerated :: Int -> IO ()
_checkEventGenerated n = replicateM_ 10 $ do
    xs <- generate (eventsFromPattern <$> genSessionPattern n)
    putStrLn $ (show . and $ checkSessionEvents n xs) ++ " : " ++ show xs

------------------------------------------------------------
-- generating randomized recv-send list

{- FOURMOLU_DISABLE -}
rsListFromPerm2n :: Int -> [Int] -> [Event]
rsListFromPerm2n n perm2n = runST $ newArray (1, n) False >>= \recvFlags -> mapM (action recvFlags) perm2n
  where
    action recvFlags v = do
        {-  v > n  = v - n
            True   = v     -}
        let t = (v - 1) `rem` n + 1
        readFlag recvFlags t >>= getEvent recvFlags t
    readFlag :: Ix i => STUArray s i Bool -> i -> ST s Bool
    readFlag = readArray
    getEvent recvFlags t hasRecv
        | hasRecv    = pure $ EvSend t
        | otherwise  = writeArray recvFlags t True $> EvRecv t
{- FOURMOLU_ENABLE -}

genRecvSendList :: Int -> Gen [Event]
genRecvSendList n = rsListFromPerm2n n <$> genPermutation (2 * n)

{- FOURMOLU_DISABLE -}
checkRecvSendList :: Int -> [Event] -> [Bool]
checkRecvSendList n evs =
    [ [ev' | ev <- evs, ev' <- get i ev] == [EvRecv i, EvSend i]
    | i <- [1 .. n]
    ]
  where
    get i ev@(EvRecv t)  = [ev | t == i]
    get i ev@(EvSend t)  = [ev | t == i]
    get _ _              = []
{- FOURMOLU_ENABLE -}

_checkRecvSendGenerated :: Int -> IO ()
_checkRecvSendGenerated n = replicateM_ 10 $ do
    xs <- generate (genRecvSendList n)
    putStrLn $ (show . and $ checkRecvSendList n xs) ++ " : " ++ show xs

------------------------------------------------------------
-- generating random permutaion - O(n)

genNarrowerIxs :: Int -> Gen [Int]
genNarrowerIxs n
    | n == 0 = return []
    | n == 1 = return [1]
    | otherwise = do
        v <- chooseInt (1, n)
        (v :) <$> genNarrowerIxs (pred n)

{- FOURMOLU_DISABLE -}
permFromIxs_ :: STUArray s Int Int -> Int -> [Int] -> ST s [Int]
permFromIxs_ vs last0 is0 = go last0 is0
  where
    go _      []    = return []
    go last_ (i:is)  = do
      v1 <- readArray vs i
      v2 <- readArray vs last_
      writeArray vs i v2
      {- vs is not modified, case i == last_, v1 == v2 -}
      (v1 :) <$> go (pred last_) is
{- FOURMOLU_ENABLE -}

permFromIxs :: Int -> [Int] -> [Int]
permFromIxs n ixs = runST $ do
    vs <- newListArray (1, n) [1 .. n]
    permFromIxs_ vs n ixs

genPermutation :: Int -> Gen [Int]
genPermutation n = permFromIxs n <$> genNarrowerIxs n

------------------------------------------------------------

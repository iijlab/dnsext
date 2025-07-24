{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SessionSpec where

import Test.Hspec

--
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import Network.Control.Recv
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)

--
import DNS.TAP.Schema (SocketProtocol (..))
import qualified DNS.ThreadStats as TStat

--
import DNS.Iterative.Server

spec :: Spec
spec = do
    waitInputSpec
    waitOutputSpec
    sessionSpec

type BS = ByteString

withVc
    :: IO (STM ())
    -> Int
    -> ((VcSession, ToSender -> IO (), IO FromX) -> VcTimer -> IO a)
    -> IO a
withVc getWaitIn micro action = do
    (vcSess@VcSession{..}, toSender, fromX) <- initVcSession getWaitIn
    withVcTimer micro (atomically $ enableVcTimeout vcTimeout_) $ action (vcSess, toSender, fromX)

waitInputSpec :: Spec
waitInputSpec = describe "session - wait VC input" $ do
    it "now" $ withVc readableNow 100_000 $ \(vcs, _, _) _ -> do
        result <- timeout 3_000_000 $ controlContinue $ vcControl vcs
        result `shouldBe` Just True
    it "wait" $ withVc waitRead 1_000_000 $ \(vcs, _, _) _ -> do
        result <- timeout 3_000_000 $ controlContinue $ vcControl vcs
        result `shouldBe` Just True
    it "timeout" $ withVc noReadable 100_000 $ \(vcs, _, _) _ -> do
        result <- timeout 3_000_000 $ controlContinue $ vcControl vcs
        result `shouldBe` Just False
  where
    readableNow = pure (pure ())
    waitRead = do
        hasInput <- newTVarIO False
        let toReadable = writeTVar hasInput True
        toReadable `afterUSec` 100_000
        pure $ guard =<< readTVar hasInput
    noReadable = pure retry

waitOutputSpec :: Spec
waitOutputSpec = describe "session - wait VC output" $ do
    let noReadable = pure retry
    it "finish - timeout" $ withVc noReadable 100_000 $ \(vcs, _, _) _ -> do
        result <- timeout 3_000_000 $ waitVcOutput vcs
        result `shouldBe` Just (Just VfTimeout)
    it "finish - eof" $ withVc noReadable 1_000_000 $ \(vcs@VcSession{..}, _, _) _ -> do
        enableVcEof vcEof_ `afterUSec` 100_000
        result <- timeout 3_000_000 $ waitVcOutput vcs
        result `shouldBe` Just (Just VfEof)
    it "finish - eof supersede timeout" $ withVc noReadable 0 $ \(vcs@VcSession{..}, _, _) _ -> do
        atomically $ enableVcEof vcEof_
        result <- timeout 3_000_000 $ waitVcOutput vcs
        result `shouldBe` Just (Just VfEof)
    it "wait pendings" $ withVc noReadable 0 $ \(vcs@VcSession{..}, _, _) _ -> do
        let uid = 1
        atomically $ addVcPending vcPendings_ uid
        result <- timeout 100_000 $ waitVcOutput vcs
        result `shouldBe` Nothing {- expect outside timeout-}
    it "next" $ withVc noReadable 1_000_000 $ \(vcs, toSender, _) _ -> do
        _ <- forkIO $ threadDelay 100_000 >> toSender (Output "hello" noPendingOp {- dummy -} dummyPeer)
        result <- timeout 3_000_000 $ waitVcOutput vcs
        result `shouldBe` Just Nothing

afterUSec :: STM () -> Int -> IO ()
afterUSec stm delay = void $ forkIO $ do
    threadDelay delay
    atomically stm

sessionSpec :: Spec
sessionSpec = describe "session - run VC" $ do
    it "finish 1" $ do
        m <- timeout 3_000_000 $ vcSession (pure $ pure ()) 5_000_000 ["60", "40", "20"]
        m `shouldBe` Just ((VfEof, VfEof), True)
    it "finish 2" $ do
        m <- timeout 3_000_000 $ vcSession (pure $ pure ()) 5_000_000 ["60", "40", "20", "60", "40", "20", "60", "40", "20"]
        m `shouldBe` Just ((VfEof, VfEof), True)
    it "timeout" $ do
        m <- timeout 3_000_000 $ vcSession (pure retry) 100_000 ["20"]
        m `shouldBe` Just ((VfTimeout, VfTimeout), False)
    it "wait slow" $ do
        m <- timeout 3_000_000 $ vcSession (pure $ pure ()) 50_000 ["100"]
        m `shouldBe` Just ((VfEof, VfEof), True)

vcSession :: IO (STM ()) -> Int -> [ByteString] -> IO ((VcFinished, VcFinished), Bool)
vcSession waitRead tmicro ws = do
    recv <- getRecv ws
    (fstate, result) <- runSession 1000 recv waitRead tmicro
    let expect = sort ws
    pure (fstate, result == expect)

---

{- FOUMOLU_DISABLE -}
runSession :: Int -> IO (ByteString, Peer) -> IO (STM ()) -> Int -> IO ((VcFinished, VcFinished), [ByteString])
runSession factor recv0 waitRead tmicro = withVc waitRead tmicro $ \(vcSess, toSender, fromX) timer -> do
    env <- newEmptyEnv
    toCacher <- getToCacher factor
    (getResult, send0) <- getSend
    debug <- maybe False ((== "1") . take 1) <$> lookupEnv "VCTEST_DEBUG"
    let myaddr = SockAddrInet 53 0x0100007f
        recv = do
            bp@(bs, _) <- recv0
            checkReceived 0 timer bs
            return bp
        send = getSendVC timer send0
        receiver = receiverVC "test-recv" env vcSess recv toCacher (mkInput myaddr toSender UDP)
        sender = senderVC "test-send" env vcSess send fromX
    when debug $ void $ forkIO $ replicateM_ 10 $ do
        {- dumper to debug -}
        dump vcSess
        threadDelay 500_000

    fstate <- TStat.concurrently "test-send" sender "test-recv" receiver
    result <- sort <$> getResult
    pure (fstate, result)

{- FOUMOLU_ENABLE -}

dump :: VcSession -> IO ()
dump VcSession{..} = do
    (e, p, a) <- atomically $ (,,) <$> readTVar vcEof_ <*> readTVar vcPendings_ <*> vcRespAvail_
    putStrLn $ unwords ["eof:", show e, "pendings:", show p, "avail:", show a]

{- FOUMOLU_DISABLE -}
getToCacher :: Int -> IO (ToCacher -> IO ())
getToCacher factor = do
    mq <- newTQueueIO
    let bodyLoop = forever $ do
            Input{..} <- atomically (readTQueue mq)
            let intb = fromMaybe 0 $ readMaybe $ B8.unpack inputQuery
            threadDelay $ intb * factor
            inputToSender $ Output inputQuery inputPendingOp inputPeerInfo
    replicateM_ 4 (forkIO bodyLoop)
    pure (atomically . writeTQueue mq)

{- FOUMOLU_ENABLE -}

getRecv :: [ByteString] -> IO (IO (BS, Peer))
getRecv ws = do
    ref <- newIORef ws
    pure $ rstep ref
  where
    rstep ref = do
        s <- readIORef ref
        case s of
            [] -> pure (mempty, dummyPeer)
            c : cs -> writeIORef ref cs $> (c, dummyPeer)

getSend :: IO (IO [ByteString], BS -> Peer -> IO ())
getSend = do
    ref <- newIORef []
    pure (readIORef ref, \x _ -> sstep ref x)
  where
    ins x s = (x : s, ())
    sstep ref x = atomicModifyIORef' ref (ins x)

dummyPeer :: Peer
dummyPeer = PeerInfoVC $ SockAddrInet 12345 0x0100007f

module LogSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import GHC.IO.Handle (hDuplicate)
import System.IO (hClose, stdout)
import System.Timeout (timeout)
import Test.Hspec

import qualified DNS.Log as Log

spec :: Spec
spec = describe "logger" $
    it "goes on draining the queue once its handle fails" $ do
        h <- hDuplicate stdout
        (ops, lowOps) <-
            Log.newHandleLogger (pure id) (pure h) hClose Log.WARNING $ \o l -> pure (o, l)
        void $ forkIO $ Log.runLogger lowOps
        -- Every write fails from here on.
        hClose h
        let logAndStop = do
                mapM_ (\i -> Log.putLines ops Log.WARNING Nothing [show i]) [1 .. 10 :: Int]
                Log.stopLogger lowOps
        timeout 2000000 logAndStop `shouldReturn` Just ()

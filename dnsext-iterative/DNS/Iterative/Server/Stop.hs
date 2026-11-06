-- | Stopping a server by telling it.
module DNS.Iterative.Server.Stop (
    stoppable,
) where

-- GHC packages
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (join, void)

-- dnsext-utils packages
import qualified DNS.ThreadAsync as TAsync

-- | Running a server with an installer of the action which stops it.
--   The installed action is run once the 'STM' action fires.
stoppable :: STM () -> ((IO () -> IO ()) -> IO a) -> IO a
stoppable stop server = do
    var <- newEmptyMVar
    TAsync.withAsync "bw.stopper" (atomically stop >> join (readMVar var)) $ \_ ->
        server $ void . tryPutMVar var

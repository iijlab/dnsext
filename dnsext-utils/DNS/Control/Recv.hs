{-# LANGUAGE RecordWildCards #-}

module DNS.Control.Recv (
    -- * Controlled receiving
    Check,
    Control,
    controlContinue,
    newControl,
    Terminate (..),
    withControlledRecv,
    getLeftover,

    -- * Internal
    controlledRecv,
    Result (..),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef

-- | Return 'True' when receiving data is possible.
type Check = IO Bool

-- | Control data for a receiving function.
data Control = Control
    { controlContinue :: Check
    , controlBuilder :: IORef (Int, [ByteString] -> [ByteString])
    }

-- | Creating 'Control'.
newControl
    :: Check
    -> IO Control
newControl controlContinue = do
    controlBuilder <- newIORef (0, id)
    return Control{..}

-- | The reason why the receiving function is terminated.
data Terminate
    = -- | End of file.
      EOF
    | -- | When 'Check' returns 'False', 'Break' is retuned. 'Break'
      --   is timeout in the normal case.
      Break
    deriving (Eq, Show)

-- | Result.
data Result
    = Terminate Terminate
    | NotEnough
    | NBytes ByteString
    deriving (Eq, Show)

-- | Controlled receiving function.
controlledRecv :: Control -> (Int -> IO ByteString) -> Int -> IO Result
controlledRecv Control{..} recvN len = do
    cont <- controlContinue
    (blen, builder) <- readIORef controlBuilder
    if cont
        then do
            let wantN = len - blen
            bs <- recvN wantN
            let n = BS.length bs
            if n == 0
                then return $ Terminate EOF
                else do
                    let builder' = builder . (bs :)
                    if n == wantN
                        then do
                            let finalBS = BS.concat $ builder' []
                            writeIORef controlBuilder (0, id)
                            return $ NBytes finalBS
                        else do
                            let blen' = blen + n
                            writeIORef controlBuilder (blen', builder')
                            return NotEnough
        else return $ Terminate Break

-- | Use to get leftover for 'Terminate'.
getLeftover :: Control -> IO ByteString
getLeftover Control{..} = do
    (_blen, builder) <- readIORef controlBuilder
    let leftover = BS.concat $ builder []
    return leftover

-- | Calling an action with 'ByteString' of the exact size.
withControlledRecv
    :: Control
    -> (Int -> IO ByteString)
    -- ^ Receiving function
    -> Int
    -- ^ How many bytes are wanted.
    -> (ByteString -> IO a)
    -- ^ An action which receives 'ByteString' of the exact size.
    -> IO (Either Terminate a)
withControlledRecv ctl recvN len action = go
  where
    go = do
        r <- controlledRecv ctl recvN len
        case r of
            NotEnough -> go
            Terminate t -> return $ Left t
            NBytes bs -> Right <$> action bs

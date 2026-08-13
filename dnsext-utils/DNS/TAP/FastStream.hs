{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Fast stream implementaion.
--
-- * Spec: <https://github.com/farsightsec/fstrm/blob/master/fstrm/control.h>
module DNS.TAP.FastStream (
    -- * Types
    Config (..),
    Context,
    newReaderContext,
    newWriterContext,

    -- * Reader and writer
    reader,
    writer,

    -- * API
    handshake,
    recvData,
    sendData,
    bye,
) where

import Control.Exception (Exception (..))
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.Word
import Network.ByteOrder
import Network.Socket
import qualified Network.Socket.BufferPool as P
import qualified Network.Socket.ByteString as NSB

----------------------------------------------------------------

-- | Configuration for fast stream.
data Config = Config
    { bidirectional :: Bool
    , debug :: Bool
    }

----------------------------------------------------------------

-- | Context of a connection.
data Context = Context
    { ctxRecv :: Int -> IO ByteString
    , ctxSend :: [ByteString] -> IO ()
    , ctxBidi :: Bool
    , ctxReader :: Bool
    , ctxDebug :: Bool
    }

-- | Creating 'Context' from 'Socket'.
newContext :: Bool -> Socket -> Config -> IO Context
newContext isReader s conf = do
    pool <- P.newBufferPool 512 16384
    recvN <- P.makeRecvN "" $ P.receive s pool
    return
        Context
            { ctxRecv = recvN
            , ctxSend = NSB.sendMany s
            , ctxBidi = bidirectional conf
            , ctxReader = isReader
            , ctxDebug = debug conf
            }

-- | Creating 'Context' for readers from 'Socket'.
newReaderContext :: Socket -> Config -> IO Context
newReaderContext = newContext True

-- | Creating 'Context' for writers from 'Socket'.
newWriterContext :: Socket -> Config -> IO Context
newWriterContext = newContext False

----------------------------------------------------------------

newtype Control = Control {fromControl :: Word32} deriving (Eq)

{- FOURMOLU_DISABLE -}
pattern ESCAPE :: Control
pattern ESCAPE  = Control 0x00
pattern ACCEPT :: Control
pattern ACCEPT  = Control 0x01
pattern START  :: Control
pattern START   = Control 0x02
pattern STOP   :: Control
pattern STOP    = Control 0x03
pattern READY  :: Control
pattern READY   = Control 0x04
pattern FINISH :: Control
pattern FINISH  = Control 0x05

instance Show Control where
    show ESCAPE = "ESCAPE"
    show ACCEPT = "ACCEPT"
    show START  = "START"
    show STOP   = "STOP"
    show READY  = "READY"
    show FINISH = "FINISH"
    show (Control n) = "Control " ++ show n
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

newtype FieldType = FieldType {fromFieldType :: Word32} deriving (Eq, Show)

pattern ContentType :: FieldType
pattern ContentType = FieldType 0x01

-- | content type of frame stream (fstrm) used for dnstap
--
-- I couldn't find the specification for this but found several
-- implementations include unbound use this value as a content type.
dnsTapContentType :: ByteString
dnsTapContentType = "protobuf:dnstap.Dnstap"

----------------------------------------------------------------

newtype FSException = FSException String deriving (Show)

instance Exception FSException

----------------------------------------------------------------

recvLength :: Context -> IO Word32
recvLength Context{..} = do
    bsc <- ctxRecv 4
    unsafeWithByteString bsc peek32

recvControl :: Context -> IO Control
recvControl Context{..} = do
    bsc <- ctxRecv 4
    Control <$> unsafeWithByteString bsc peek32

recvContent :: Context -> Word32 -> IO ByteString
recvContent Context{..} l = ctxRecv $ fromIntegral l

----------------------------------------------------------------

recvControlFrame :: Context -> Control -> Bool -> IO [(FieldType, ByteString)]
recvControlFrame ctx@Context{..} ctrl skipEscape = do
    unless skipEscape $ do
        c <- recvControl ctx
        check c ESCAPE
    l0 <- recvLength ctx
    when (l0 < 4) $ E.throwIO $ FSException "illegal control length"
    c <- recvControl ctx
    check c ctrl
    when ctxDebug $ print ctrl
    let l1 = l0 - 4
    loop l1 id
  where
    loop 0 build = return $ build []
    loop l build = do
        when (l < 8) $ E.throwIO $ FSException "illegal field length"
        ft <- FieldType <$> recvLength ctx
        l0 <- recvLength ctx
        ct <- recvContent ctx l0
        if ft == ContentType
            then do
                when ctxDebug $ do
                    putStr "Content-Type: "
                    C8.putStrLn ct
            else when ctxDebug $ putStrLn "unknown field"
        loop (l - 8 - l0) (build . ((ft, ct) :))

check :: Control -> Control -> IO ()
check c ctrl = when (c /= ctrl) $ E.throwIO $ FSException ("no " ++ show ctrl)

sendControlFrame :: Context -> Control -> [(FieldType, ByteString)] -> IO ()
sendControlFrame Context{..} ctrl xs = do
    let esc = bytestring32 $ fromControl ESCAPE
        ctr = bytestring32 $ fromControl ctrl
        xss = concatMap enc xs
        len = bytestring32 $ fromIntegral (4 + sum (map C8.length xss))
    ctxSend (esc : len : ctr : xss)
  where
    enc (t, c) =
        [ bytestring32 $ fromFieldType t
        , bytestring32 $ fromIntegral $ C8.length c
        , c
        ]

----------------------------------------------------------------
-- API

-- | Setting up the connection.
handshake :: Context -> IO ()
handshake ctx@Context{..}
    | ctxReader = do
        when ctxBidi $ do
            ct <- recvControlFrame ctx READY False
            -- fixme: select one
            sendControlFrame ctx ACCEPT ct
        void $ recvControlFrame ctx START False
    | otherwise = do
        when ctxBidi $ do
            sendControlFrame ctx READY [(ContentType, dnsTapContentType)]
            void $ recvControlFrame ctx ACCEPT False
        sendControlFrame ctx START [(ContentType, dnsTapContentType)]

-- | Receiving data.
--   "" indicates that writer stops writing.
recvData :: Context -> IO ByteString
recvData ctx@Context{..}
    | ctxReader = do
        l <- recvLength ctx
        when ctxDebug $ putStrLn "--------------------------------"
        if l == 0 -- ESCAPE
            then return ""
            else do
                when ctxDebug $ putStrLn $ "fstrm data length: " ++ show l
                recvContent ctx l
    | otherwise = E.throwIO $ FSException "client cannot use recvData"

-- | Writing data.
sendData :: Context -> ByteString -> IO ()
sendData Context{..} bs
    | ctxReader = E.throwIO $ FSException "server cannot use sendData"
    | otherwise = do
        let len = bytestring32 $ fromIntegral $ C8.length bs
        ctxSend [len, bs]

-- | Tearing down the connection.
bye :: Context -> IO ()
bye ctx@Context{..}
    | ctxReader = do
        void $ recvControlFrame ctx STOP True
        when ctxBidi $ sendControlFrame ctx FINISH [] `E.catch` ignore
    | otherwise = do
        sendControlFrame ctx STOP []
        when ctxBidi $ void $ recvControlFrame ctx FINISH False
  where
    -- SomeException: asynchronous exceptions are re-thrown
    ignore :: E.SomeException -> IO ()
    ignore se
        | Just (E.SomeAsyncException _) <- E.fromException se = E.throwIO se
        | otherwise = return ()

----------------------------------------------------------------

-- | Reading loop.
--   The loop is finished when the writer stops writing.
reader :: Socket -> Config -> (ByteString -> IO ()) -> IO ()
reader sock conf body = E.bracket open bye loop
  where
    open = do
        ctx <- newReaderContext sock conf
        handshake ctx
        return ctx
    loop ctx = do
        bs <- recvData ctx
        if bs == ""
            then return ()
            else do
                body bs
                loop ctx

-- | Writing loop.
--   Generating an empty string stops the loop.
writer :: Socket -> Config -> IO ByteString -> IO ()
writer sock conf body = E.bracket open bye loop
  where
    open = do
        ctx <- newWriterContext sock conf
        handshake ctx
        return ctx
    loop ctx = do
        bs <- body
        if bs == ""
            then return ()
            else do
                sendData ctx bs
                loop ctx

{-# LANGUAGE DeriveFunctor #-}

module DNS.Types.Dict (
    getRData,
    getOData,
    extendRR,
    extendOpt,
    InitIO,
    runInitIO,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as M
import System.IO.Unsafe (unsafePerformIO)

import DNS.Types.EDNS
import DNS.Types.Opaque.Internal
import DNS.Types.RData
import DNS.Types.Type
import DNS.Wire

----------------------------------------------------------------

{-# NOINLINE globalRDataDict #-}
globalRDataDict :: IORef RDataDict
globalRDataDict = unsafePerformIO $ newIORef defaultRDataDict

{-# NOINLINE globalODataDict #-}
globalODataDict :: IORef ODataDict
globalODataDict = unsafePerformIO $ newIORef defaultODataDict

addRData :: TYPE -> (Int -> Parser RData) -> IO ()
addRData typ dec = atomicModifyIORef' globalRDataDict f
  where
    f dict = (M.insert (toKey typ) dec dict, ())

addOData :: OptCode -> (Int -> Parser OData) -> IO ()
addOData code dec = atomicModifyIORef' globalODataDict f
  where
    f dict = (M.insert (toKeyO code) dec dict, ())

----------------------------------------------------------------

getRData :: TYPE -> Int -> Parser RData
getRData OPT len rbuf ref = rd_opt <$> sGetMany "EDNS option" len getoption rbuf ref
  where
    dict = unsafePerformIO $ readIORef globalODataDict
    getoption _ _ = do
        code <- toOptCode <$> get16 rbuf
        olen <- getInt16 rbuf
        getOData dict code olen rbuf ref
getRData typ len rbuf ref = case M.lookup (toKey typ) dict of
    Nothing -> rd_unknown typ <$> getOpaque len rbuf ref
    Just dec -> dec len rbuf ref
  where
    dict = unsafePerformIO $ readIORef globalRDataDict

----------------------------------------------------------------

type RDataDict = M.IntMap (Int -> Parser RData)

toKey :: TYPE -> M.Key
toKey = fromIntegral . fromTYPE

{- FOURMOLU_DISABLE -}
defaultRDataDict :: M.IntMap (Int -> Parser RData)
defaultRDataDict =
    M.fromList
        [ (toKey A      , getRD get_a)
        , (toKey NS     , getRD get_ns)
        , (toKey CNAME  , getRD get_cname)
        , (toKey SOA    , getRD get_soa)
        , (toKey NULL   , getRD get_null)
        , (toKey PTR    , getRD get_ptr)
        , (toKey MX     , getRD get_mx)
        , (toKey TXT    , getRD get_txt)
        , (toKey RP     , getRD get_rp)
        , (toKey AAAA   , getRD get_aaaa)
        , (toKey SRV    , getRD get_srv)
        , (toKey DNAME  , getRD get_dname)
        , (toKey TLSA   , getRD get_tlsa)
        , (toKey TSIG   , getRD get_tsig)
        ]
  where
    getRD get_x len rbuf ref = toRData <$> get_x len rbuf ref
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

type ODataDict = M.IntMap (Int -> Parser OData)

getOData :: ODataDict -> OptCode -> Int -> Parser OData
getOData dict code len rbuf ref = case M.lookup (toKeyO code) dict of
    Nothing -> od_unknown (fromOptCode code) <$> getOpaque len rbuf ref
    Just dec -> dec len rbuf ref

toKeyO :: OptCode -> M.Key
toKeyO = fromIntegral . fromOptCode

{- FOURMOLU_DISABLE -}
defaultODataDict :: ODataDict
defaultODataDict =
    M.fromList
        [ (toKeyO NSID          , getOD get_nsid)
        , (toKeyO ClientSubnet  , getOD get_clientSubnet)
        , (toKeyO Padding       , getOD get_padding)
        , (toKeyO EDNSError     , getOD get_ednsError)
        ]
  where
    getOD get_x len rbuf ref = toOData <$> get_x len rbuf ref
{- FOURMOLU_ENABLE -}

----------------------------------------------------------------

extendRR :: TYPE -> String -> (Int -> Parser RData) -> InitIO ()
extendRR typ name proxy = InitIO $ do
    addRData typ proxy
    addType typ name

extendOpt :: OptCode -> String -> (Int -> Parser OData) -> InitIO ()
extendOpt code name proxy = InitIO $ do
    addOData code proxy
    addOpt code name

----------------------------------------------------------------

newtype InitIO a = InitIO
    { runInitIO :: IO a
    }
    deriving (Functor)

instance Applicative InitIO where
    pure x = InitIO $ pure x
    InitIO x <*> InitIO y = InitIO (x <*> y)

instance Monad InitIO where
    m >>= f = InitIO $ do
        x <- runInitIO m
        runInitIO $ f x

instance MonadIO InitIO where
    liftIO = InitIO

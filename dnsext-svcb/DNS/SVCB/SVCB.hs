{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module DNS.SVCB.SVCB where

import DNS.SVCB.Imports
import DNS.SVCB.Key
import DNS.SVCB.Params
import DNS.SVCB.Value
import DNS.Types
import DNS.Types.Internal
import qualified DNS.Types.Opaque as Opaque
import qualified Data.IntMap.Strict as M

----------------------------------------------------------------

pattern SVCB :: TYPE
pattern SVCB = TYPE 64

pattern HTTPS :: TYPE
pattern HTTPS = TYPE 65

----------------------------------------------------------------

data RD_SVCB = RD_SVCB
    { svcb_priority :: Word16
    , svcb_target :: Domain
    , svcb_params :: SvcParams
    }
    deriving (Eq, Ord)

instance Show RD_SVCB where
    show RD_SVCB{..} = show svcb_priority ++ " " ++ show svcb_target ++ " " ++ show svcb_params

instance ResourceData RD_SVCB where
    resourceDataType _ = SVCB
    resourceDataSize RD_SVCB{..} = 2 + domainSize svcb_target + siz
      where
        SvcParams m = svcb_params
        siz = foldr (\(SvcParamValue o) s -> s + 4 + Opaque.length o) 0 m
    putResourceData cf RD_SVCB{..} = \wbuf ref -> do
        put16 wbuf svcb_priority
        -- https://datatracker.ietf.org/doc/html/draft-ietf-dnsop-svcb-https-11#section-2.2
        -- "the uncompressed, fully-qualified TargetName"
        putDomain cf svcb_target wbuf ref
        let SvcParams m = svcb_params
        void $ M.foldrWithKey (f wbuf ref) (return ()) m
      where
        f wbuf ref k v x = encodekv k v wbuf ref >> x
        encodekv k (SvcParamValue v) wbuf ref = do
            putInt16 wbuf k
            putInt16 wbuf $ Opaque.length v
            putOpaque v wbuf ref

get_svcb :: Int -> Parser RD_SVCB
get_svcb len rbuf ref = do
    end <- (+) len <$> position rbuf
    priority <- get16 rbuf
    target <- getDomain rbuf ref
    pos <- position rbuf
    params <- newSvcParams <$> sGetMany "SVCB Param" (end - pos) svcparam rbuf ref
    return $ RD_SVCB priority target params
  where
    svcparam _ _ = do
        key <- getInt16 rbuf -- intestinally parsing as Int
        lng <- getInt16 rbuf
        val <- getOpaque lng rbuf ref
        return (key, SvcParamValue val)

rd_svcb :: Word16 -> Domain -> SvcParams -> RData
rd_svcb p d s = toRData $ RD_SVCB p d s

----------------------------------------------------------------

data RD_HTTPS = RD_HTTPS
    { https_priority :: Word16
    , https_target :: Domain
    , https_params :: SvcParams
    }
    deriving (Eq, Ord)

instance Show RD_HTTPS where
    show RD_HTTPS{..} = show https_priority ++ " " ++ show https_target ++ " " ++ show https_params

instance ResourceData RD_HTTPS where
    resourceDataType _ = HTTPS
    resourceDataSize (RD_HTTPS x y z) = resourceDataSize $ RD_SVCB x y z
    putResourceData cf (RD_HTTPS x y z) = putResourceData cf $ RD_SVCB x y z

get_https :: Int -> Parser RD_HTTPS
get_https len rbuf ref = do
    RD_SVCB x y z <- get_svcb len rbuf ref
    return $ RD_HTTPS x y z

rd_https :: Word16 -> Domain -> SvcParams -> RData
rd_https p d s = toRData $ RD_HTTPS p d s

----------------------------------------------------------------

addResourceDataForSVCB :: InitIO ()
addResourceDataForSVCB = do
    extendRR SVCB "SVCB" (\len rbuf ref -> toRData <$> get_svcb len rbuf ref)
    extendRR HTTPS "HTTPS" (\len rbuf ref -> toRData <$> get_https len rbuf ref)

----------------------------------------------------------------

-- | Look up and decode a parameter value.
extractSvcParam :: SPV v => SvcParamKey -> SvcParams -> Maybe v
extractSvcParam key params = lookupSvcParam key params >>= fromSvcParamValue

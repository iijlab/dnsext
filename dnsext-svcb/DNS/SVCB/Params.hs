module DNS.SVCB.Params where

import DNS.SVCB.Key
import DNS.SVCB.Value
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

newtype SvcParams = SvcParams (IntMap SvcParamValue) deriving (Eq, Ord)

instance Show SvcParams where
    show (SvcParams m) = unwords $ M.foldrWithKey f [] m
      where
        showkv k v =
            show (toSvcParamKey $ fromIntegral k)
                ++ "="
                ++ showValue (toSvcParamKey $ fromIntegral k) v
        f k v xs = showkv k v : xs

showValue :: SvcParamKey -> SvcParamValue -> String
showValue SPK_Port v = case fromSvcParamValue v of
    Nothing -> ""
    Just x@(SPV_Port _) -> show x
showValue SPK_IPv4Hint v = case fromSvcParamValue v of
    Nothing -> ""
    Just x@(SPV_IPv4Hint _) -> show x
showValue SPK_IPv6Hint v = case fromSvcParamValue v of
    Nothing -> ""
    Just x@(SPV_IPv6Hint _) -> show x
showValue SPK_ALPN v = case fromSvcParamValue v of
    Nothing -> ""
    Just x@(SPV_ALPN _) -> show x
showValue SPK_DoHPath v = case fromSvcParamValue v of
    Nothing -> ""
    Just x@(SPV_DoHPath _) -> show x
showValue SPK_ECH v = case fromSvcParamValue v of
    Nothing -> ""
    Just x@(SPV_ECH _) -> show x
showValue _ v = show v

lookupSvcParam :: SvcParamKey -> SvcParams -> Maybe SvcParamValue
lookupSvcParam key (SvcParams m) = M.lookup k m
  where
    k = fromIntegral $ fromSvcParamKey key

newSvcParams :: [(Int, SvcParamValue)] -> SvcParams
newSvcParams kvs = SvcParams $ foldr ins M.empty kvs
  where
    ins (k, v) = M.insert k v

toSvcParams :: [(SvcParamKey, SvcParamValue)] -> SvcParams
toSvcParams kvs = SvcParams $ foldr ins M.empty kvs
  where
    ins (SvcParamKey k, v) = M.insert (fromIntegral k) v

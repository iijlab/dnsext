{-# LANGUAGE FlexibleContexts #-}

module DNS.ZoneFile.ParserSVCB where

-- GHC
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Short as Short
import Data.Char (chr)
import Data.Functor
import Data.List
import Data.String (fromString)
import Data.Word
import qualified Data.Word8 as W8
import Text.Read (readMaybe)

-- dnsext-*
import DNS.SVCB hiding (TYPE)
import DNS.SVCB.Internal
import DNS.Types (Domain, RData, TYPE)
import qualified DNS.Types.Opaque as Opaque

-- this package
import DNS.Parser
import DNS.ZoneFile.ParserBase
import DNS.ZoneFile.Types

rdatasSVCB :: MonadParser Token s m => m Domain -> [(TYPE, m RData)]
rdatasSVCB dom =
    [ (HTTPS, rdataSVCB dom)
    , (SVCB, rdataSVCB dom)
    ]

-- $setup
-- >>> :seti -XTypeApplications
-- >>> import qualified Data.ByteString as BS
-- >>> import Data.Char (ord)
-- >>> type BS = BS.ByteString
-- >>> runParser' = runParser @BS
-- >>> esstr cs = [C (fromIntegral $ ord c) | c <- cs]

rdataSVCB :: MonadParser Token s m => m Domain -> m RData
rdataSVCB dom = rd_svcb <$> svcPriority <*> (blank *> dom) <*> params
  where
    params = toSvcParams <$> svcParams

-- | The SvcParams of one record.
--
--   RFC 9460 Sec 2.1: in presentation format they may be written in any
--   order, but "SvcParamKeys SHALL NOT be repeated".  They are held in
--   a map, so a repeated key used to lose one of its two values without
--   a word: a zone file saying @alpn=h2 alpn=h3@ loaded and served one
--   of them.
--
--   A repeated key ends the list here rather than being refused where
--   it is found, so what the reader is shown is the token left over --
--   which is the second of the pair -- in the same way as every other
--   thing wrong with a SvcParam.
svcParams :: MonadParser Token s m => m [(SvcParamKey, SvcParamValue)]
svcParams = go []
  where
    go seen = next seen <|> pure []
    next seen = do
        kv@(key, _) <- blank *> svcParam
        when (key `elem` seen) $
            parseError $
                "zone-svcb: SvcParamKey given more than once: " ++ show key
        (kv :) <$> go (key : seen)

---

{-
SvcPriority TargetName SvcParams

The SVCB record is defined specifically within the Internet ("IN") Class ([RFC1035], Section 3.2.4).

SvcPriority is a number in the range 0-65535, TargetName is a <domain-name> ([RFC1035], Section 5.1),
and the SvcParams are a whitespace-separated list with each SvcParam consisting of a SvcParamKey=SvcParamValue pair or a standalone SvcParamKey. SvcParamKeys are registered by IANA

alpha-lc      = %x61-7A   ; a-z
SvcParamKey   = 1*63(alpha-lc / DIGIT / "-")
SvcParam      = SvcParamKey ["=" SvcParamValue]
SvcParamValue = char-string ; See Appendix A.
value         = *OCTET ; Value before key-specific parsing
 -}

---

svcPriority :: MonadParser Token s m => m Word16
svcPriority = readCString "svc_priority"

svcParam :: MonadParser Token s m => m (SvcParamKey, SvcParamValue)
svcParam = esToSvcParam . concatMap cs_es =<< some (cstring' <|> dot $> fromString ".")

{- FOURMOLU_DISABLE -}
-- |
-- >>> type Parsed a = Either String (SvcParamKey, a)
-- >>> svcp k v = maybe (Left "no-value") (Right . (,) k) $ fromSvcParamValue v
-- >>> parseSVCP s = runParser' (esToSvcParam $ esstr s) mempty >>= \((k, v), _) -> svcp k v
--
-- >>> parseSVCP "mandatory=alpn,ipv4hint" :: Parsed SPV_Mandatory
-- Right (mandatory,[alpn,ipv4hint])
-- >>> parseSVCP "alpn=foo,bar" :: Parsed SPV_ALPN
-- Right (alpn,["foo","bar"])
-- >>> parseSVCP "port=1234" :: Parsed SPV_Port
-- Right (port,1234)
-- >>> parseSVCP "ipv4hint=192.0.2.17,192.0.2.18" :: Parsed SPV_IPv4Hint
-- Right (ipv4hint,[192.0.2.17,192.0.2.18])
-- >>> parseSVCP "ipv6hint=2001:db8::11,2001:db8::12" :: Parsed SPV_IPv6Hint
-- Right (ipv6hint,[2001:db8::11,2001:db8::12])
-- >>> parseSVCP "dohpath=/dns-query{?dns}" :: Parsed SPV_DoHPath
-- Right (dohpath,"/dns-query{?dns}")
esToSvcParam :: MonadParser t s m => EString -> m (SvcParamKey, SvcParamValue)
esToSvcParam es = splitKeyStr es invalidK $ \sk sv -> do
    (k, getV) <- svcParamKey sk
    v <- either parseError pure $ getV sv
    pure (k, v)
  where invalidK = parseError $ "zone-svcb: SvcParamKey: cannot get svcb-key: " ++ show es
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
splitKeyStr :: EString -> a -> (String -> Maybe EString -> a) -> a
splitKeyStr es0 error_ result = go id es0
  where
    cases k  []                                       = result k  Nothing
    cases k (C w8 : es)
        | w8 == W8._equal && 1 <= klen && klen <= 63  = result k (Just es)
      where klen = length k
    cases _  _                                        = error_
    kcs = '-' : ['0' .. '9'] ++ ['a' .. 'z']
    go a      []     =          cases (a []) []
    go a ees@(e:es)  = case e of
        C w8 | c `elem` kcs  -> go (a . (c:))  es
           where c = chr (fromIntegral w8)
        _                    -> cases (a []) ees
{- FOURMOLU_ENABLE -}

svcParamKey :: MonadParser t s m => String -> m (SvcParamKey, Maybe EString -> Either String SvcParamValue)
svcParamKey name = maybe unknown pure $ getSvcParamKey name
  where
    unknown = parseError $ "zone-svcb: SvcParamKey: unknown key name: " ++ name

{- FOURMOLU_DISABLE -}
getSvcParamKey :: String -> Maybe (SvcParamKey, Maybe EString -> Either String SvcParamValue)
getSvcParamKey name = lookup name knownSvcParamKeys <|> keyNNNNN
  where
    keyNNNNN = do
        sn <- stripPrefix "key" name
        -- https://datatracker.ietf.org/doc/html/rfc9460#section-2.1
        -- 'the unknown-key presentation format "keyNNNNN" where NNNNN is the numeric value of the key type without leading zeros.'
        guard (take 1 sn /= "0")
        k <- readMaybe sn
        Just (toSvcParamKey k, pure . maybe (valueOpaque []) valueOpaque)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | SvcParamKey data table:
--   https://www.iana.org/assignments/dns-svcb/dns-svcb.xhtml
knownSvcParamKeys :: [(String, (SvcParamKey, Maybe EString -> Either String SvcParamValue))]
knownSvcParamKeys =
    [ ("mandatory"             , (SPK_Mandatory          , just "mandatory"  valueMandatory              ))
    , ("alpn"                  , (SPK_ALPN               , just "alpm"       valueALPN                   ))
    , ("no-default-alpn"       , (SPK_NoDefaultALPN      , nothing "no-default-alpn" valueNoDefaultALPN  ))
    , ("port"                  , (SPK_Port               , just "port"       valuePort                   ))
    , ("ipv4hint"              , (SPK_IPv4Hint           , just "ipv4hint"   valueIPv4Hint               ))
    -- , ("ech"                   , (SPK_ECH                , just "ech"        valueECH                    ))
    , ("ipv6hint"              , (SPK_IPv6Hint           , just "ipv6hint"   valueIPv6Hint               ))
    , ("dohpath"               , (SPK_DoHPath            , just "dohpath"  $ Right . valueDoHPath        ))
    --- , ("ohttp"                 , undefined)
    --- , ("tls-supported-groups"  , undefined)
    ]
  where
    just name       = maybe (Left $ "zone-svcb: " ++ name ++ ": value required")
    nothing name v  = maybe v (const $ Left $ "zone-svcb: " ++ name ++ ": must be empty value")
{- FOURMOLU_ENABLE -}

---

valueOpaque :: EString -> SvcParamValue
valueOpaque es = toSvcParamValue $ SPV_Opaque $ Opaque.fromShortByteString $ Short.pack $ map unEscW8 es

valueMandatory :: EString -> Either String SvcParamValue
valueMandatory es = toSvcParamValue . SPV_Mandatory <$> (mapM (resolvKey . map (w8chr . unEscW8)) =<< onComma es)
  where
    resolvKey kn = maybe (Left $ "zone-svcb: mandatory: cannot resolve key: " ++ kn) (Right . fst) $ getSvcParamKey kn

valueALPN :: EString -> Either String SvcParamValue
valueALPN es = mkALPN <$> onComma es
  where
    mkALPN xs = toSvcParamValue $ SPV_ALPN [Short.pack $ map unEscW8 e | e <- xs]

valueNoDefaultALPN :: Either String SvcParamValue
valueNoDefaultALPN = Right $ SvcParamValue mempty

valuePort :: EString -> Either String SvcParamValue
valuePort es = do
    let left s = Left $ "zone-svcb: port: " ++ s
    port <- maybe (left $ "not digit char: " ++ show es) Right $ mapM digitChar es
    let plen = length port
    maybe (left $ "out of range: " ++ port) Right $ guard (1 <= plen && plen <= 5)
    maybe (left $ "fail to read: " ++ port) (Right . toSvcParamValue . SPV_Port) $ readMaybe port

valueIPv4Hint :: EString -> Either String SvcParamValue
valueIPv4Hint es = toSvcParamValue . SPV_IPv4Hint <$> (mapM getV4 =<< onComma es)
  where
    digitOrDot w8 = W8.isDigit w8 || w8 == W8._period
    getV4' e = readMaybe =<< mapM (esatisfyChar digitOrDot) e
    getV4 e = maybe (Left $ "zone-svcb: ipv4hint: not IPv4 pattern: " ++ show e) Right (getV4' e)

-- valueECH :: EString -> Either String SvcParamValue
-- valueECH es = do
--     s <- maybe (Left $ "zone-svcb: ech: " ++ show es) Right $ mapM (esatisfyChar $ const True) es
--     o <- either (Left . ("zone-svcb: ech: base64: " ++)) Right $ Opaque.fromBase64 $ B8.pack s
--     Right $ SvcParamValue o

valueIPv6Hint :: EString -> Either String SvcParamValue
valueIPv6Hint es = toSvcParamValue . SPV_IPv6Hint <$> (mapM getV6 =<< onComma es)
  where
    digitOrColon w8 = W8.isHexDigit w8 || w8 == W8._colon
    getV6' e = readMaybe =<< mapM (esatisfyChar digitOrColon) e
    getV6 e = maybe (Left $ "zone-svcb: ipv6hint: not IPv6 pattern: " ++ show e) Right (getV6' e)

valueDoHPath :: EString -> SvcParamValue
valueDoHPath es = toSvcParamValue . SPV_DoHPath $ Short.pack $ map unEscW8 es

{- FOURMOLU_DISABLE -}
-- https://datatracker.ietf.org/doc/html/rfc9460#section-7.2
-- https://datatracker.ietf.org/doc/html/rfc9460#section-7.3
-- this SvcParamValue MUST NOT contain escape sequences.
digitChar :: Word8E -> Maybe Char
digitChar = esatisfyChar W8.isDigit
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
esatisfyChar :: (Word8 -> Bool) -> Word8E -> Maybe Char
esatisfyChar p (C w8)
   | p w8              = Just (w8chr w8)
esatisfyChar _  _      = Nothing
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
onComma :: EString -> Either String [EString]
onComma = go
  where
    go es = do
        m <- get1 es
        let next (x, es1) = (x :) <$> go es1
        maybe (Right []) next m
    get1 [] =             Right   Nothing
    get1 es = case tl of
        []             -> Right $ Just (hd, [])
        _ : []         -> Left "zone-svcb: SvcParamValue: comma-separated list, trailing comma is invalid"
        _ : es1@(_:_)  -> Right $ Just (hd, es1)
      where
        (hd, tl) = break (== C W8._comma) es
{- FOURMOLU_ENABLE -}

---

w8chr :: Word8 -> Char
w8chr = chr . fromIntegral

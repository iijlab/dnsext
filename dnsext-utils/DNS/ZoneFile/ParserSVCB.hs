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
-- >>> w8str cs = [fromIntegral (ord c) :: Word8 | c <- cs]

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
svcParam = toSvcParam . concatMap Short.unpack =<< some (cstring <|> dot $> fromString ".")

{- FOURMOLU_DISABLE -}
-- | A SvcParam from the bytes of its presentation, the character-string decoding done.
--
-- >>> type Parsed a = Either String (SvcParamKey, a)
-- >>> svcp k v = maybe (Left "no-value") (Right . (,) k) $ fromSvcParamValue v
-- >>> parseSVCP s = runParser' (toSvcParam $ w8str s) mempty >>= \((k, v), _) -> svcp k v
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
--
-- RFC 9460 Appendix D.2 Figure 10, @alpn="f\\\\oo\\,bar,h2"@, decoded to @alpn=f\\oo\,bar,h2@:
--
-- >>> parseSVCP "alpn=f\\\\oo\\,bar,h2" :: Parsed SPV_ALPN
-- Right (alpn,["f\\oo,bar","h2"])
toSvcParam :: MonadParser t s m => [Word8] -> m (SvcParamKey, SvcParamValue)
toSvcParam ws = splitKeyStr ws invalidK $ \sk sv -> do
    (k, getV) <- svcParamKey sk
    v <- either parseError pure $ getV sv
    pure (k, v)
  where invalidK = parseError $ "zone-svcb: SvcParamKey: cannot get svcb-key: " ++ show (map w8chr ws)
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
splitKeyStr :: [Word8] -> a -> (String -> Maybe [Word8] -> a) -> a
splitKeyStr ws0 error_ result = go id ws0
  where
    cases k  []                                       = result k  Nothing
    cases k (w8 : ws)
        | w8 == W8._equal && 1 <= klen && klen <= 63  = result k (Just ws)
      where klen = length k
    cases _  _                                        = error_
    kcs = '-' : ['0' .. '9'] ++ ['a' .. 'z']
    go a      []       =  cases (a []) []
    go a wws@(w8:ws)
        | c `elem` kcs  = go (a . (c:))  ws
        | otherwise     = cases (a []) wws
      where c = w8chr w8
{- FOURMOLU_ENABLE -}

svcParamKey :: MonadParser t s m => String -> m (SvcParamKey, Maybe [Word8] -> Either String SvcParamValue)
svcParamKey name = maybe unknown pure $ getSvcParamKey name
  where
    unknown = parseError $ "zone-svcb: SvcParamKey: unknown key name: " ++ name

{- FOURMOLU_DISABLE -}
getSvcParamKey :: String -> Maybe (SvcParamKey, Maybe [Word8] -> Either String SvcParamValue)
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
knownSvcParamKeys :: [(String, (SvcParamKey, Maybe [Word8] -> Either String SvcParamValue))]
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

valueOpaque :: [Word8] -> SvcParamValue
valueOpaque ws = toSvcParamValue $ SPV_Opaque $ Opaque.fromShortByteString $ Short.pack ws

valueMandatory :: [Word8] -> Either String SvcParamValue
valueMandatory ws = toSvcParamValue . SPV_Mandatory <$> (mapM (resolvKey . map w8chr) =<< valueList ws)
  where
    resolvKey kn = maybe (Left $ "zone-svcb: mandatory: cannot resolve key: " ++ kn) (Right . fst) $ getSvcParamKey kn

valueALPN :: [Word8] -> Either String SvcParamValue
valueALPN ws = mkALPN <$> valueList ws
  where
    mkALPN xs = toSvcParamValue $ SPV_ALPN [Short.pack x | x <- xs]

valueNoDefaultALPN :: Either String SvcParamValue
valueNoDefaultALPN = Right $ SvcParamValue mempty

valuePort :: [Word8] -> Either String SvcParamValue
valuePort ws = do
    let left s = Left $ "zone-svcb: port: " ++ s
    port <- maybe (left $ "not digit char: " ++ show (map w8chr ws)) Right $ mapM (satisfyChar W8.isDigit) ws
    let plen = length port
    maybe (left $ "out of range: " ++ port) Right $ guard (1 <= plen && plen <= 5)
    maybe (left $ "fail to read: " ++ port) (Right . toSvcParamValue . SPV_Port) $ readMaybe port

valueIPv4Hint :: [Word8] -> Either String SvcParamValue
valueIPv4Hint ws = toSvcParamValue . SPV_IPv4Hint <$> (mapM getV4 =<< valueList ws)
  where
    digitOrDot w8 = W8.isDigit w8 || w8 == W8._period
    getV4' x = readMaybe =<< mapM (satisfyChar digitOrDot) x
    getV4 x = maybe (Left $ "zone-svcb: ipv4hint: not IPv4 pattern: " ++ show (map w8chr x)) Right (getV4' x)

-- valueECH :: [Word8] -> Either String SvcParamValue
-- valueECH ws = do
--     o <- either (Left . ("zone-svcb: ech: base64: " ++)) Right $ Opaque.fromBase64 $ B8.pack $ map w8chr ws
--     Right $ SvcParamValue o

valueIPv6Hint :: [Word8] -> Either String SvcParamValue
valueIPv6Hint ws = toSvcParamValue . SPV_IPv6Hint <$> (mapM getV6 =<< valueList ws)
  where
    digitOrColon w8 = W8.isHexDigit w8 || w8 == W8._colon
    getV6' x = readMaybe =<< mapM (satisfyChar digitOrColon) x
    getV6 x = maybe (Left $ "zone-svcb: ipv6hint: not IPv6 pattern: " ++ show (map w8chr x)) Right (getV6' x)

valueDoHPath :: [Word8] -> SvcParamValue
valueDoHPath ws = toSvcParamValue . SPV_DoHPath $ Short.pack ws

{- FOURMOLU_DISABLE -}
satisfyChar :: (Word8 -> Bool) -> Word8 -> Maybe Char
satisfyChar p w8
   | p w8       = Just (w8chr w8)
   | otherwise  = Nothing
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- | The items of a comma-separated SvcParamValue (RFC 9460 Appendix A.1), split after
--   character-string decoding: only @\,@ and @\\@ are escapes, and no item is empty.
--
-- >>> items = fmap (map (map w8chr)) . valueList . w8str
-- >>> items "part1,part2,part3\\,part4\\\\"
-- Right ["part1","part2","part3,part4\\"]
-- >>> items "a,b"
-- Right ["a","b"]
-- >>> items ""
-- Right []
-- >>> items "a,"
-- Left "zone-svcb: SvcParamValue: comma-separated list, empty item"
-- >>> items ",a"
-- Left "zone-svcb: SvcParamValue: comma-separated list, empty item"
-- >>> items "a,,b"
-- Left "zone-svcb: SvcParamValue: comma-separated list, empty item"
-- >>> items "a\\xb"
-- Left "zone-svcb: SvcParamValue: comma-separated list, only a comma or a backslash may be escaped"
-- >>> items "a\\"
-- Left "zone-svcb: SvcParamValue: comma-separated list, only a comma or a backslash may be escaped"
valueList :: [Word8] -> Either String [[Word8]]
valueList []   = Right []
valueList ws0  = go id ws0
  where
    go a ws = case ws of
        []                              -> item (a []) (Right [])
        w : ws'
            | w == W8._comma            -> item (a []) (go id ws')
            | w == W8._backslash        -> escaped a ws'
            | otherwise                 -> go (a . (w :)) ws'
    escaped a (w : ws')
        | w == W8._comma || w == W8._backslash  = go (a . (w :)) ws'
    escaped _ _  = Left "zone-svcb: SvcParamValue: comma-separated list, only a comma or a backslash may be escaped"
    item []  _   = Left "zone-svcb: SvcParamValue: comma-separated list, empty item"
    item x   r   = (x :) <$> r
{- FOURMOLU_ENABLE -}

---

w8chr :: Word8 -> Char
w8chr = chr . fromIntegral

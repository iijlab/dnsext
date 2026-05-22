module DNS.SEC.Verify.EdDSA (
    ed25519,
    ed448,
    ed25519toPubKey,
    ed448toPubKey,
)
where

import Crypto.Error (CryptoFailable (..), onCryptoFailure)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed448 as Ed448
import Crypto.Random.Types (MonadRandom)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)

import DNS.SEC.Types (PubKey (..))
import DNS.SEC.Verify.Types
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

{- Verify RRSIG with DNSKEY using Edwards-Curve Digital Security Algorithm (EdDSA)
-- https://datatracker.ietf.org/doc/html/rfc6605
 -}

ed25519toPubKey :: PriKey -> Either String PubKey
ed25519toPubKey pri = eddsaEncodePubKey . Ed25519.toPublic <$> eddsaDecodePriKey Ed25519.secretKey pri

ed448toPubKey :: PriKey -> Either String PubKey
ed448toPubKey pri = eddsaEncodePubKey . Ed448.toPublic <$> eddsaDecodePriKey Ed448.secretKey pri

ed25519 :: RRSIGImpl
ed25519 =
    eddsaHelper
        "Ed25519"
        Ed25519.secretKey
        Ed25519.publicKey
        Ed25519.signature
        Ed25519.sign
        Ed25519.verify
        Ed25519.generateSecretKey
        Ed25519.toPublic

ed448 :: RRSIGImpl
ed448 =
    eddsaHelper
        "Ed448"
        Ed448.secretKey
        Ed448.publicKey
        Ed448.signature
        Ed448.sign
        Ed448.verify
        Ed448.generateSecretKey
        Ed448.toPublic

eddsaHelper
    :: (BA.ByteArrayAccess prikey, BA.ByteArrayAccess pubkey, BA.ByteArrayAccess sig)
    => String
    -> (ByteString -> CryptoFailable prikey)
    -> (ByteString -> CryptoFailable pubkey)
    -> (ByteString -> CryptoFailable sig)
    -> (prikey -> pubkey -> ByteString -> sig) -- pubkey is not used
    -> (pubkey -> ByteString -> sig -> Bool)
    -> IO prikey
    -> (prikey -> pubkey)
    -> RRSIGImpl
eddsaHelper algName toPri toPub toSig signImpl verifyImpl gen fromPri =
    RRSIGImpl
        { rrsigIGenKeyPair = eddsaGenKeyPair gen fromPri
        , rrsigIEncodePriKey = eddsaEncodePrikey
        , rrsigIDecodePriKey = eddsaDecodePriKey toPri
        , rrsigIEncodePubKey = eddsaEncodePubKey
        , rrsigIDecodePubKey = eddsaDecodePubKey algName toPub
        , rrsigIEncodeSignature = eddsaEncodeSignature
        , rrsigIDecodeSignature = eddsaDecodeSignature algName toSig
        , rrsigISign = eddsaSign signImpl
        , rrsigIVerify = eddsaVerify verifyImpl
        }

eddsaGenKeyPair :: MonadRandom m => m prikey -> (prikey -> pubkey) -> m (pubkey, prikey)
eddsaGenKeyPair gen fromPri = do
    pri <- gen
    let pub = fromPri pri
    return (pub, pri)

eddsaEncodePrikey :: BA.ByteArrayAccess pri => pri -> PriKey
eddsaEncodePrikey = BA.convert

eddsaDecodePriKey :: (ByteString -> CryptoFailable prikey) -> PriKey -> Either String prikey
eddsaDecodePriKey toPri pri = case toPri pri of
    CryptoFailed e -> Left $ show e
    CryptoPassed s -> Right s

eddsaEncodePubKey :: BA.ByteArrayAccess pubkey => pubkey -> PubKey
eddsaEncodePubKey = PubKey . Opaque.fromByteString . BA.convert

eddsaDecodePubKey
    :: String
    -> (ByteString -> CryptoFailable pubkey)
    -> PubKey
    -> Either String pubkey
eddsaDecodePubKey algName toPub (PubKey ks) =
    eitherCryptoFailable (algName ++ ".publicKey") . toPub $
        Opaque.toByteString ks

eddsaEncodeSignature :: BA.ByteArrayAccess sig => sig -> Opaque
eddsaEncodeSignature = Opaque.fromByteString . BA.convert

eddsaDecodeSignature
    :: String -> (ByteString -> CryptoFailable sig) -> Opaque -> Either String sig
eddsaDecodeSignature algName toSig =
    eitherCryptoFailable (algName ++ ".signature")
        . toSig
        . Opaque.toByteString

eddsaSign
    :: MonadRandom m
    => (prikey -> pubkey -> ByteString -> sig)
    -> prikey
    -> ByteString
    -> m sig
eddsaSign signImpl prikey bs = return $ signImpl prikey undefined {- not used -} bs

eddsaVerify
    :: (pubkey -> ByteString -> sig -> Bool)
    -> pubkey
    -> sig
    -> ByteString
    -> Either String Bool
eddsaVerify verifyImpl pubkey sig msg = Right $ verifyImpl pubkey msg sig

eitherCryptoFailable :: String -> CryptoFailable a -> Either String a
eitherCryptoFailable prefix = onCryptoFailure (Left . ((prefix ++ ": ") ++) . show) Right

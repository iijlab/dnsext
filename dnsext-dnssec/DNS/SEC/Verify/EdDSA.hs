module DNS.SEC.Verify.EdDSA (
    ed25519,
    ed448,
)
where

import Crypto.Error (CryptoFailable, onCryptoFailure)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Crypto.PubKey.Ed448 as Ed448
import DNS.SEC.Types (PubKey (..))
import DNS.SEC.Verify.Types
import DNS.Types
import qualified DNS.Types.Opaque as Opaque
import Data.ByteString (ByteString)

{- Verify RRSIG with DNSKEY using Edwards-Curve Digital Security Algorithm (EdDSA)
-- https://datatracker.ietf.org/doc/html/rfc6605
 -}

ed25519 :: RRSIGImpl
ed25519 = eddsaHelper "Ed25519" Ed25519.publicKey Ed25519.signature Ed25519.verify

ed448 :: RRSIGImpl
ed448 = eddsaHelper "Ed448" Ed448.publicKey Ed448.signature Ed448.verify

eddsaHelper
    :: String
    -> (ByteString -> CryptoFailable pubkey)
    -> (ByteString -> CryptoFailable sig)
    -> (pubkey -> ByteString -> sig -> Bool)
    -> RRSIGImpl
eddsaHelper algName consPublicKey consSignature verifyImpl =
    RRSIGImpl
        { rrsigIDecodePubKey = eddsaDecodePubKey algName consPublicKey
        , rrsigIDecodeSignature = eddsaDecodeSignature algName consSignature
        , rrsigIVerify = eddsaVerify verifyImpl
        }

eddsaDecodePubKey
    :: String
    -> (ByteString -> CryptoFailable pubkey)
    -> PubKey
    -> Either String pubkey
eddsaDecodePubKey algName consPublicKey (PubKey ks) =
    eitherCryptoFailable (algName ++ ".publicKey") . consPublicKey $
        Opaque.toByteString ks

eddsaDecodeSignature
    :: String -> (ByteString -> CryptoFailable sig) -> Opaque -> Either String sig
eddsaDecodeSignature algName consSignature =
    eitherCryptoFailable (algName ++ ".signature")
        . consSignature
        . Opaque.toByteString

eddsaVerify
    :: (pubkey -> ByteString -> sig -> Bool)
    -> pubkey
    -> sig
    -> ByteString
    -> Either String Bool
eddsaVerify verifyImpl pubkey sig msg = Right $ verifyImpl pubkey msg sig

eitherCryptoFailable :: String -> CryptoFailable a -> Either String a
eitherCryptoFailable prefix = onCryptoFailure (Left . ((prefix ++ ": ") ++) . show) Right

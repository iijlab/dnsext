module DNS.SEC.Verify.SHA (
    sha1,
    sha256,
    sha384,
)
where

-- ram

-- crypton
import Crypto.Hash (HashAlgorithm, hashWith)
import Crypto.Hash.Algorithms (SHA1 (..), SHA256 (..), SHA384 (..))
import DNS.SEC.Verify.Types
import qualified Data.ByteArray as BA

sha1, sha256, sha384 :: DSImpl
sha1 = shaHelper SHA1
sha256 = shaHelper SHA256
sha384 = shaHelper SHA384

shaHelper :: HashAlgorithm hash => hash -> DSImpl
shaHelper hash =
    DSImpl
        { dsIGetDigest = BA.convert . hashWith hash
        , dsIVerify = verify
        }
  where
    verify digest bs = digest == bs

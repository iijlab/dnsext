{-# LANGUAGE OverloadedStrings #-}

module DNS.Iterative.RootTrustAnchors (
    rootSepDSs,
    rootSepDS,
) where

import DNS.SEC
import qualified DNS.Types.Opaque as Opaque

{- import trust-anchor DS RData from
   https://data.iana.org/root-anchors/root-anchors.xml -}
rootSepDSs :: [RD_DS]
rootSepDSs = [RD_DS 20326 RSASHA256 SHA256 digest]
  where
    digest =
        either (error . ("rootSepDSs: bad configuration: " ++)) id $
            Opaque.fromBase16
                "E06D44B80B8F1D39A95C0B0D7C65D08458E880409BBC683457104237C7F8EC8D"

{-# DEPRECATED rootSepDS "use rootSepDSs instead of this" #-}
rootSepDS :: RD_DS
rootSepDS = RD_DS 20326 RSASHA256 SHA256 digest
  where
    digest =
        either (error . ("rootSepDS: bad configuration: " ++)) id $
            Opaque.fromBase16
                "E06D44B80B8F1D39A95C0B0D7C65D08458E880409BBC683457104237C7F8EC8D"

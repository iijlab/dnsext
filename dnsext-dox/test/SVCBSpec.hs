{-# LANGUAGE OverloadedStrings #-}

module SVCBSpec where

import DNS.Do53.Internal
import DNS.DoX.Client
import DNS.SVCB
import DNS.Types
import Data.Maybe (fromJust)
import Test.Hspec

spec :: Spec
spec = describe "what a SVCB record says a server is called" $ do
    it "takes the target name, without the dot a certificate has not got" $
        serverNames (svcb "dns.example.") `shouldBe` [Just "dns.example"]

    -- RFC 9460 Sec 2.5: in ServiceMode a TargetName of "." stands for
    -- the owner name of the record itself, and for DDR that owner is
    -- _dns.resolver.arpa, which no certificate is for.  So such a
    -- record names no server a certificate could be checked against.
    --
    -- The empty string is not a way of saying that.  It is a name, and
    -- one nothing is called: it goes out as an empty SNI and as an
    -- empty ":authority" in every DoH request.
    it "says a target of the root names nobody" $
        serverNames (svcb ".") `shouldBe` [Nothing]

-- | A ServiceMode SVCB record offering one ALPN and nothing else.
svcb :: Domain -> RD_SVCB
svcb target =
    fromJust $ fromRData $ rd_svcb 1 target $ toSvcParams [(SPK_ALPN, spv_alpn ["dot"])]

-- | The name every 'ResolveInfo' the record turns into would
--   authenticate its server by.
serverNames :: RD_SVCB -> [Maybe String]
serverNames s =
    [ rinfoServerName ri
    | sis <- svcbResolveInfos tag defaultResolveInfo [s]
    , si <- sis
    , ri <- svcbInfoResolveInfos si
    ]
  where
    tag = nameTag defaultResolveInfo{rinfoIP = "192.0.2.1"} "dot"

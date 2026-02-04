{-# LANGUAGE OverloadedStrings #-}

module RoundTripSpec (spec) where

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as Short
import Data.Either (fromRight)
import Data.IP (Addr, IP (..), IPv4, IPv6, makeAddrRange, toIPv4, toIPv6)
import qualified Data.IP
import Data.String (fromString)
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (
    Gen,
    arbitrary,
    elements,
    forAll,
    frequency,
    listOf,
    oneof,
 )

import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import DNS.Types.Internal
import qualified DNS.Types.Opaque as Opaque

spec :: Spec
spec = do
    prop "IPv4" . forAll genIPv4 $ \ip4 -> do
        let str = show ip4
        read str `shouldBe` ip4
        show (read str :: IPv4) `shouldBe` str

    prop "IPv6" . forAll genIPv6 $ \ip6 -> do
        let str = show ip6
        read str `shouldBe` ip6
        show (read str :: IPv6) `shouldBe` str

    prop "TYPE" . forAll genTYPE $ \t ->
        toTYPE (fromTYPE t) `shouldBe` t

    prop "Domain" . forAll genDomain $ \dom -> do
        let bs = encodeDomain dom
        decodeDomain bs `shouldBe` Right dom
        fmap encodeDomain (decodeDomain bs) `shouldBe` Right bs

    prop "Mailbox" . forAll genMailbox $ \dom -> do
        let bs = encodeMailbox dom
        decodeMailbox bs `shouldBe` Right dom
        fmap encodeMailbox (decodeMailbox bs) `shouldBe` Right bs

    prop "DNSFlags" . forAll (genDNSFlagsStuff 0x0f) $ \x -> do
        let bs = encodeDNSFlags x
        decodeDNSFlags bs `shouldBe` Right x
        encodeDNSFlags <$> decodeDNSFlags bs `shouldBe` Right bs

    prop "ResourceRecord" . forAll genResourceRecord $ \rr -> do
        let bs = encodeResourceRecord rr
        decodeResourceRecord bs `shouldBe` Right rr
        fmap encodeResourceRecord (decodeResourceRecord bs) `shouldBe` Right bs

    prop "DNSMessage" . forAll genDNSMessage $ \msg ->
        decode (encode msg) `shouldBe` Right msg

    --    prop "DNSMessage" . forAll genDNSMessage $ \msg ->
    --        let inps = map BS.singleton $ BS.unpack $ encode msg
    --         in decodeChunks 3426660848 inps `shouldBe` Right msg

    prop "EDNS" . forAll genEDNSHeader $ \(edns, (fl, op, rc)) -> do
        let eh = EDNSheader edns
            m =
                fromRight (error "prop EDNS") $ decode $ encode $ DNSMessage 1 op rc fl eh defaultQuestion [] [] []
        ednsHeader m `shouldBe` eh

----------------------------------------------------------------

genDNSMessage :: Gen DNSMessage
genDNSMessage = do
    DNSMessage
        <$> genWord16
        <*> genOPCODE
        <*> genRCODE 0x0f
        <*> genDNSFlags
        <*> makeEDNS
        <*> genQuestion
        <*> listOf genResourceRecord
        <*> listOf genResourceRecord
        <*> listOf genResourceRecord
  where
    makeEDNS :: Gen EDNSheader
    makeEDNS =
        genBool >>= \t ->
            if t
                then EDNSheader <$> genEDNS
                else pure NoEDNS

genQuestion :: Gen Question
genQuestion = Question <$> genDomain <*> genTYPE <*> pure IN

genTYPE :: Gen TYPE
genTYPE =
    frequency
        [
            ( 20
            , elements
                [A, AAAA, NS, TXT, MX, CNAME, SOA, PTR, SRV, DNAME, OPT, TLSA]
            )
        , (1, toTYPE <$> genWord16)
        ]

genResourceRecord :: Gen ResourceRecord
genResourceRecord =
    frequency
        [ (8, genRR)
        ]
  where
    genRR = do
        dom <- genDomain
        t <- elements [A, AAAA, NS, TXT, MX, CNAME, SOA, PTR, SRV, DNAME, TLSA]
        ResourceRecord dom t IN <$> genSeconds <*> mkRData dom t

mkRData :: Domain -> TYPE -> Gen RData
mkRData dom typ =
    case typ of
        A -> rd_a <$> genIPv4
        AAAA -> rd_aaaa <$> genIPv6
        NS -> pure $ rd_ns dom
        TXT -> rd_txt_n <$> genTextStrings
        MX -> rd_mx <$> genWord16 <*> genDomain
        CNAME -> pure $ rd_cname dom
        SOA ->
            rd_soa dom
                <$> genMailbox
                <*> genWord32
                <*> genSeconds
                <*> genSeconds
                <*> genSeconds
                <*> genSeconds
        PTR -> rd_ptr <$> genDomain
        SRV -> rd_srv <$> genWord16 <*> genWord16 <*> genWord16 <*> genDomain
        DNAME -> rd_dname <$> genDomain
        TLSA -> rd_tlsa <$> genWord8 <*> genWord8 <*> genWord8 <*> genOpaque
        _ -> pure . rd_txt $ fromString ("Unhandled type " <> show typ)
  where
    genTextStrings = do
        chunks <- elements [1 .. 5] -- chunk=0 is not llegal one or more <character-sstring>s, RFC 1035, RFC 6763
        replicateM chunks genTextString
    genTextString = do
        len <- elements [0, 1, 63, 127, 255]
        Opaque.fromShortByteString . Short.pack <$> replicateM len genWord8

genIPv4 :: Gen IPv4
genIPv4 = toIPv4 <$> replicateM 4 (fromIntegral <$> genWord8)

genIPv6 :: Gen IPv6
genIPv6 = toIPv6 <$> replicateM 8 (fromIntegral <$> genWord16)

genOpaque :: Gen Opaque
genOpaque =
    Opaque.fromByteString
        <$> elements ["", "a", "a.b", "abc", "a.b.c", "a\\.b.c", "\\001.a.b", "\\$.a.b"]

genDomain :: Gen Domain
genDomain =
    elements
        [".", "a.", "a.b.", "abc.", "a.b.c.", "a\\.b.c.", "\\001.a.b.", "\\$.a.b."]

genMailbox :: Gen Mailbox
genMailbox = elements ["a@b.", "a@b.c.", "first.last@example.org."]

genDNSFlagsStuff :: Word16 -> Gen (DNSFlags, OPCODE, RCODE)
genDNSFlagsStuff maxrc = do
    fl <- genDNSFlags
    op <- genOPCODE
    rc <- genRCODE maxrc
    return (fl, op, rc)

genDNSFlags :: Gen DNSFlags
genDNSFlags =
    DNSFlags
        <$> genBool
        <*> genBool
        <*> genBool
        <*> genBool
        <*> genBool
        <*> genBool
        <*> genBool

genWord16 :: Gen Word16
genWord16 = arbitrary

genWord32 :: Gen Word32
genWord32 = arbitrary

genSeconds :: Gen Seconds
genSeconds = Seconds <$> genWord32

genWord8 :: Gen Word8
genWord8 = arbitrary

genBool :: Gen Bool
genBool = elements [True, False]

genOPCODE :: Gen OPCODE
genOPCODE = elements [OP_STD, OP_INV, OP_SSR, OP_NOTIFY, OP_UPDATE]

genRCODE :: Word16 -> Gen RCODE
genRCODE maxrc = elements $ map toRCODE [0 .. maxrc]

genEDNS :: Gen EDNS
genEDNS = do
    vers <- genWord8
    ok <- genBool
    od <- genOData
    us <- elements [minUdpSize .. maxUdpSize]
    return $
        defaultEDNS
            { ednsVersion = vers
            , ednsUdpSize = us
            , ednsDnssecOk = ok
            , ednsOptions = [od]
            }

genOData :: Gen OData
genOData =
    oneof
        [ genOD_Unknown
        , genOD_ECS
        ]
  where
    -- \| Choose from the range reserved for local use
    -- https://tools.ietf.org/html/rfc6891#section-9
    genOD_Unknown = od_unknown <$> elements [65001, 65534] <*> genOpaque

    -- \| Only valid ECS prefixes round-trip, make sure the prefix is
    -- is consistent with the mask.
    genOD_ECS = do
        usev4 <- genBool
        if usev4
            then genFuzzed genIPv4 IPv4 Data.IP.fromIPv4 1 32
            else genFuzzed genIPv6 IPv6 Data.IP.fromIPv6b 2 128
      where
        genFuzzed
            :: Addr a
            => Gen a
            -> (a -> IP)
            -> (a -> [Int])
            -> Word16
            -> Word8
            -> Gen OData
        genFuzzed gen toIP toBytes fam alen = do
            ip <- gen
            bits1 <- elements [1 .. alen]
            bits2 <- elements [0 .. alen]
            fuzzSrcBits <- genBool
            fuzzScpBits <- genBool
            srcBits <-
                if not fuzzSrcBits
                    then pure bits1
                    else flip mod alen . (+) bits1 <$> elements [1 .. alen - 1]
            scpBits <-
                if not fuzzScpBits
                    then pure bits2
                    else elements [alen + 1 .. 0xFF]
            let addr = Data.IP.addr . makeAddrRange ip $ fromIntegral bits1
                bytes = map fromIntegral $ toBytes addr
                len = (fromIntegral bits1 + 7) `div` 8
                less = take (len - 1) bytes
                more = less ++ [0xFF]
            if srcBits == bits1
                then
                    if scpBits == bits2
                        then pure $ od_clientSubnet bits1 scpBits $ toIP addr
                        else pure $ od_ecsGeneric fam bits1 scpBits $ Opaque.fromByteString $ BS.pack bytes
                else
                    if srcBits < bits1
                        then pure $ od_ecsGeneric fam srcBits scpBits $ Opaque.fromByteString $ BS.pack more
                        else pure $ od_ecsGeneric fam srcBits scpBits $ Opaque.fromByteString $ BS.pack less

genEDNSHeader :: Gen (EDNS, (DNSFlags, OPCODE, RCODE))
genEDNSHeader = do
    edns <- genEDNS
    hdr <- genDNSFlagsStuff 0xF00
    return (edns, hdr)

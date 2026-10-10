{-# LANGUAGE OverloadedStrings #-}

-- | Which NSEC and NSEC3 records may be used to prove what.  RFC 6840
--   Sec 4.1 and Sec 4.3 name two kinds which say less than they look
--   like they say.
module NonexistenceSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List (sortOn)
import Data.String (fromString)
import Test.Hspec

import DNS.SEC
import DNS.SEC.Internal
import DNS.SEC.Verify
import DNS.Types
import qualified DNS.Types.Opaque as Opaque

spec :: Spec
spec = do
    runIO $ runInitIO addResourceDataForDNSSEC
    describe "an NSEC which is used to prove what it cannot" $ do
        -- The RFC 4035 Appendix B.3 case, unchanged, so that what
        -- follows is the bitmap and nothing else.
        it "proves a NODATA where it is the name's own NSEC" $
            nsecNoData [("ns1.example.", nsec "ns2.example." [A, RRSIG, NSEC])] "ns1.example." MX
                `shouldSatisfy` isRight

        -- RFC 6840 Sec 4.3: "validators MUST check the CNAME bit".  A
        -- name with a CNAME has every type at it, so a NODATA for one
        -- of them is an answer the CNAME was stripped out of.
        it "will not prove a NODATA at a name which has a CNAME" $
            nsecNoData [("ns1.example.", nsec "ns2.example." [CNAME, RRSIG, NSEC])] "ns1.example." MX
                `shouldSatisfy` isLeft

        -- RFC 6840 Sec 4.1: NS set and SOA clear is the delegation
        -- point as the parent holds it, and it says nothing about what
        -- the child has.
        it "will not prove a NODATA at a delegation it does not hold" $
            nsecNoData [("b.example.", nsec "ns1.example." [NS, RRSIG, NSEC])] "b.example." MX
                `shouldSatisfy` isLeft

        -- Except for the DS, which is the parent's to deny.
        it "still proves a NODATA for the DS at that delegation" $
            nsecNoData [("b.example.", nsec "ns1.example." [NS, RRSIG, NSEC])] "b.example." DS
                `shouldSatisfy` isRight

        -- The RFC 4035 Appendix B.2 case, unchanged.
        it "proves a name error with records which enclose the name" $
            nsecNameError
                [ ("b.example.", nsec "ns1.example." [NS, RRSIG, NSEC])
                , ("example.", nsec "a.example." [NS, SOA, MX, RRSIG, NSEC, DNSKEY])
                ]
                "ml.example."
                `shouldSatisfy` isRight

        -- RFC 6840 Sec 4.1 again: an ancestor delegation cannot be
        -- used for a name below the cut.  `ml.b.example.` is below
        -- `b.example.`, which is a delegation the parent holds.
        it "will not prove a name error below a delegation" $
            nsecNameError
                [ ("b.example.", nsec "ns1.example." [NS, RRSIG, NSEC])
                , ("example.", nsec "a.example." [NS, SOA, MX, RRSIG, NSEC, DNSKEY])
                ]
                "ml.b.example."
                `shouldSatisfy` isLeft

    describe "an NSEC3 which is used to prove what it cannot" $ do
        -- The RFC 5155 Appendix B.2 case, unchanged.
        it "proves a NODATA where it is the name's own NSEC3" $
            nsec3NoData [(h "2t7b4g4vsa5smi47k61mv5bv1a22bojr", n3 "2vptu5timamqttgl4luu9kg21e0aor3s" [A, RRSIG])] "ns1.example." MX
                `shouldSatisfy` isRight

        it "will not prove a NODATA at a name which has a CNAME" $
            nsec3NoData [(h "2t7b4g4vsa5smi47k61mv5bv1a22bojr", n3 "2vptu5timamqttgl4luu9kg21e0aor3s" [CNAME, RRSIG])] "ns1.example." MX
                `shouldSatisfy` isLeft

        it "will not prove a NODATA at a delegation it does not hold" $
            nsec3NoData [(h "2t7b4g4vsa5smi47k61mv5bv1a22bojr", n3 "2vptu5timamqttgl4luu9kg21e0aor3s" [NS, RRSIG])] "ns1.example." MX
                `shouldSatisfy` isLeft

        it "still proves a NODATA for the DS at that delegation" $
            nsec3NoData [(h "2t7b4g4vsa5smi47k61mv5bv1a22bojr", n3 "2vptu5timamqttgl4luu9kg21e0aor3s" [NS, RRSIG])] "ns1.example." DS
                `shouldSatisfy` isRight

        -- The RFC 5155 Appendix B.1 case, unchanged.
        it "proves a name error with a closest encloser and the rest" $
            nsec3NameError rfc5155NameError "a.c.x.w.example." `shouldSatisfy` isRight

        -- RFC 6840 Sec 4.1: the closest encloser is where the name
        -- stops existing, and a delegation point there means the rest
        -- of the name is the child's business.
        it "will not prove a name error whose closest encloser is a delegation" $
            nsec3NameError (closestWith [NS, RRSIG]) "a.c.x.w.example." `shouldSatisfy` isLeft

        -- And a DNAME there means the name was redirected, not absent.
        it "will not prove a name error whose closest encloser has a DNAME" $
            nsec3NameError (closestWith [DNAME, RRSIG]) "a.c.x.w.example." `shouldSatisfy` isLeft

----------------------------------------------------------------

nsec :: Domain -> [TYPE] -> RData
nsec = rd_nsec

-- | The NSEC3 parameters of the RFC 5155 examples, whose hashes the
--   owner names below are taken from: SHA-1, Opt-Out, twelve
--   iterations, salt aabbccdd.
n3 :: String -> [TYPE] -> RData
n3 next = rd_nsec3 Hash_SHA1 [OptOut] 12 (b16 "aabbccdd") (b32 next)

b16 :: String -> Opaque
b16 = either (error "b16") id . Opaque.fromBase16 . fromString

b32 :: String -> Opaque
b32 = either (error "b32") id . Opaque.fromBase32Hex . fromString

h :: String -> Domain
h x = fromString $ x ++ ".example."

-- | RFC 5155 Appendix B.1, which the two below start from.
rfc5155NameError :: [(Domain, RData)]
rfc5155NameError = closestWith [MX, RRSIG]

-- | The same with the closest encloser's bitmap replaced.  For
--   @a.c.x.w.example.@ that is @x.w.example.@ (b4um86...995); the apex
--   is the next closer cover and the third is the wildcard cover.
closestWith :: [TYPE] -> [(Domain, RData)]
closestWith types =
    [ (h "0p9mhaveqvm6t7vbl5lop2u3t2rp3tom", n3 "2t7b4g4vsa5smi47k61mv5bv1a22bojr" [MX, DNSKEY, NS, SOA, NSEC3PARAM, RRSIG])
    , (h "b4um86eghhds6nea196smvmlo4ors995", n3 "gjeqe526plbf1g8mklp59enfd789njgi" types)
    , (h "35mthgpgcu1qg68fab165klnsnk3dpvl", n3 "b4um86eghhds6nea196smvmlo4ors995" [NS, DS, RRSIG])
    ]

nsecNoData :: [(Domain, RData)] -> Domain -> TYPE -> Either String ()
nsecNoData rds qname qtype = () <$ noDataNSEC "example." (nsecRanges rds) qname qtype

nsecNameError :: [(Domain, RData)] -> Domain -> Either String ()
nsecNameError rds qname = () <$ nameErrorNSEC "example." (nsecRanges rds) qname

nsec3NoData :: [(Domain, RData)] -> Domain -> TYPE -> Either String ()
nsec3NoData rds qname qtype = () <$ noDataNSEC3 "example." (nsec3Ranges rds) qname qtype

nsec3NameError :: [(Domain, RData)] -> Domain -> Either String ()
nsec3NameError rds qname = () <$ nameErrorNSEC3 "example." (nsec3Ranges rds) qname

nsecRanges :: [(Domain, RData)] -> [NSEC_Range]
nsecRanges rds = sortOn fst [(owner, r) | (owner, rd) <- rds, Just r <- [fromRData rd]]

nsec3Ranges :: [(Domain, RData)] -> [NSEC3_Range]
nsec3Ranges rds = sortOn fst [(owner, r) | (owner, rd) <- rds, Just r <- [fromRData rd]]

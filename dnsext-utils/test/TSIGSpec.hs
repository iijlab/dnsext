{-# LANGUAGE OverloadedStrings #-}

module TSIGSpec (spec) where

import qualified Data.ByteString as BS
import Numeric (showHex)
import Test.Hspec

import Data.Bits (xor)

import DNS.TSIG
import DNS.Types
import DNS.Types.Decode
import DNS.Types.Encode
import qualified DNS.Types.Opaque as Opaque
import DNS.Types.Time (EpochTime)

spec :: Spec
spec = do
    -- The HMACs themselves come from crypton; these say that each
    -- algorithm is wired to the one it is named after, against the
    -- vectors of RFC 2202 Sec 3 and RFC 4231 Sec 4.
    describe "the MAC of each algorithm" $ do
        it "HMAC-SHA1 matches RFC 2202 case 1" $
            mac HMAC_SHA1 (BS.replicate 20 0x0b) "Hi There"
                `shouldBe` "b617318655057264e28bc0b6fb378c8ef146be00"

        it "HMAC-SHA224 matches RFC 4231 case 1" $
            mac HMAC_SHA224 (BS.replicate 20 0x0b) "Hi There"
                `shouldBe` "896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22"

        it "HMAC-SHA256 matches RFC 4231 case 1" $
            mac HMAC_SHA256 (BS.replicate 20 0x0b) "Hi There"
                `shouldBe` "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"

        it "HMAC-SHA384 matches RFC 4231 case 2" $
            mac HMAC_SHA384 "Jefe" "what do ya want for nothing?"
                `shouldBe` "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e"
                    ++ "8e2240ca5e69e2c78b3239ecfab21649"

        it "HMAC-SHA512 matches RFC 4231 case 2" $
            mac HMAC_SHA512 "Jefe" "what do ya want for nothing?"
                `shouldBe` "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea250554"
                    ++ "9758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737"

        it "gives a MAC of the length the algorithm says" $
            mapM_
                (\a -> BS.length (macBytes a "secret" "message") `shouldBe` macLength a)
                [minBound ..]

    describe "algorithm names" $ do
        it "round trips" $
            mapM_ (\a -> algorithmFromName (algorithmName a) `shouldBe` Just a) [minBound ..]

        it "does not mind the case they are written in" $
            algorithmFromName "HMAC-SHA256." `shouldBe` Just HMAC_SHA256

        it "does not know HMAC-MD5, which RFC 8945 Sec 6 forbids" $
            algorithmFromName "hmac-md5.sig-alg.reg.int." `shouldBe` Nothing

    describe "checking a MAC" $ do
        it "accepts the MAC the key gives" $
            checkMAC key digest (tsigMAC key digest) `shouldBe` Nothing

        it "rejects one taken with another secret" $
            checkMAC key digest (tsigMAC other digest) `shouldBe` Just BADSIG

        it "rejects one taken over other octets" $
            checkMAC key digest (tsigMAC key "something else") `shouldBe` Just BADSIG

        -- RFC 8945 Sec 5.2.2.1: not shorter than ten octets nor than
        -- half the algorithm's own length, whichever is larger.  For
        -- SHA256 that is sixteen.
        it "accepts one truncated to half the hash" $
            checkMAC key digest (truncated 16) `shouldBe` Nothing

        it "rejects one truncated below half the hash" $
            checkMAC key digest (truncated 15) `shouldBe` Just BADTRUNC

        it "rejects a truncation that is wrong as BADSIG, not BADTRUNC" $
            checkMAC key digest (Opaque.fromByteString $ BS.replicate 16 0xff)
                `shouldBe` Just BADSIG

        it "will not take ten octets from a long hash" $
            -- Half of SHA512 is 32, so ten is far too few for it.
            checkMAC key512 digest (Opaque.fromByteString $ BS.replicate 10 0) `shouldBe` Just BADTRUNC

    describe "checking the time" $ do
        it "accepts a time within the fudge" $ do
            checkTime 1000 1000 300 `shouldBe` Nothing
            checkTime 1000 1300 300 `shouldBe` Nothing
            checkTime 1000 700 300 `shouldBe` Nothing

        it "rejects one outside it, either way" $ do
            checkTime 1000 1301 300 `shouldBe` Just BADTIME
            checkTime 1000 699 300 `shouldBe` Just BADTIME

    describe "error numbers" $
        it "are the ones RFC 8945 assigns" $ do
            fromTSIGError BADSIG `shouldBe` 16
            fromTSIGError BADKEY `shouldBe` 17
            fromTSIGError BADTIME `shouldBe` 18
            fromTSIGError BADTRUNC `shouldBe` 22

    -- A message signed and then checked, which is what the two halves
    -- are for.  Signing puts a record on the end of the message, so
    -- what is checked is the message with it, encoded.
    describe "signing a message and checking it again" $ do
        it "accepts what it signed" $
            verifyTSIG held now Nothing (signed Nothing plain) (decoded $ signed Nothing plain)
                `shouldBe` TSIGOk (macOf $ signed Nothing plain)

        it "accepts a response bound to its request" $
            let reqMac = macOf $ signed Nothing plain
                rsp = signed (Just reqMac) plain
             in verifyTSIG held now (Just reqMac) rsp (decoded rsp) `shouldBe` TSIGOk (macOf rsp)

        it "rejects a response checked against another request" $
            let reqMac = macOf $ signed Nothing plain
                rsp = signed (Just reqMac) plain
                wrong = Opaque.fromByteString $ BS.replicate 32 0
             in failure (verifyTSIG held now (Just wrong) rsp (decoded rsp)) `shouldBe` Just BADSIG

        it "rejects a message a byte of which was changed" $
            -- A byte of the header: changing one of a name instead is
            -- also refused, but as BADKEY, since the owner name of the
            -- record is a pointer into the question and moves with it.
            let bs = flipBit 3 $ signed Nothing plain
             in failure (verifyTSIG held now Nothing bs (decoded bs)) `shouldBe` Just BADSIG

        it "rejects one whose question was changed under it" $
            let bs = flipBit 20 $ signed Nothing plain
             in failure (verifyTSIG held now Nothing bs (decoded bs)) `shouldBe` Just BADKEY

        it "rejects one signed with another secret" $
            let bs = signedWith other Nothing plain
             in failure (verifyTSIG held now Nothing bs (decoded bs)) `shouldBe` Just BADSIG

        it "says BADKEY for a key it does not hold" $
            let bs = signedWith stranger Nothing plain
             in failure (verifyTSIG held now Nothing bs (decoded bs)) `shouldBe` Just BADKEY

        it "says BADTIME when the clocks are too far apart" $
            let bs = signed Nothing plain
             in failure (verifyTSIG held (now + 301) Nothing bs (decoded bs)) `shouldBe` Just BADTIME

        it "says nothing is there when nothing is" $
            verifyTSIG held now Nothing (encode plain) plain `shouldBe` TSIGMissing

        -- RFC 8945 Sec 5.2: a message with two of them is dropped and
        -- answered FORMERR.  There is nothing here to check, which is
        -- what a caller acts on -- not a MAC which happens not to match.
        it "will not check a message carrying two TSIGs" $
            let one = decoded $ signed Nothing plain
                two = encode $ withRR (last $ additional one) one
             in verifyTSIG held now Nothing two (decoded two) `shouldBe` TSIGMissing

        it "puts the record last, where RFC 8945 Sec 5.1 wants it" $
            (rrtype . last . additional . decoded) (signed Nothing plain) `shouldBe` TSIG

        it "keeps the identifier of the message it signs" $
            (tsig_original_id <$> tsigOf (signed Nothing plain)) `shouldBe` Just (identifier plain)

    -- RFC 8945 Sec 5.3.1: after the first, a message is bound to the
    -- one before it and to every unsigned message in between.
    describe "a chain of messages" $ do
        it "accepts a second message bound to the first" $
            let first' = signed Nothing plain
                firstMac = macOf first'
                body = encode plain
                (rr, _) = signTSIGCont key now defaultFudge firstMac [body]
                second' = encode $ withRR rr plain
             in verifyTSIGCont held now firstMac [] second' (decoded second')
                    `shouldBe` TSIGOk (macOf second')

        it "takes in the messages that carried no record" $
            let firstMac = macOf $ signed Nothing plain
                body = encode plain
                (rr, _) = signTSIGCont key now defaultFudge firstMac [body, body, body]
                third = encode $ withRR rr plain
             in verifyTSIGCont held now firstMac [body, body] third (decoded third)
                    `shouldBe` TSIGOk (macOf third)

        it "rejects one that lost a message in between" $
            let firstMac = macOf $ signed Nothing plain
                body = encode plain
                (rr, _) = signTSIGCont key now defaultFudge firstMac [body, body, body]
                third = encode $ withRR rr plain
             in failure (verifyTSIGCont held now firstMac [body] third (decoded third))
                    `shouldBe` Just BADSIG

    -- RFC 8945 Sec 5.2 and Sec 5.3.2: the answer to a TSIG which did
    -- not pass says which of the checks it was that did not pass.
    describe "the answer which says a TSIG was no good" $ do
        let strange = signedWith stranger Nothing plain
            badkey = faultOf $ verifyTSIG held now Nothing strange (decoded strange)
            wrongly = signedWith other Nothing plain
            badsig = faultOf $ verifyTSIG held now Nothing wrongly (decoded wrongly)
            -- A request from a client whose clock is five hundred
            -- seconds behind ours.
            late = signed Nothing plain
            badtime = faultOf $ verifyTSIG held (now + 500) Nothing late (decoded late)
            server = now + 500

        it "names the key it was asked for, and the algorithm" $ do
            let rr = lastRR $ refusal badkey now
            rrname rr `shouldBe` tsigKeyName stranger
            rrclass rr `shouldBe` CL_ANY
            (tsig_algorithm <$> tsigOf (refusal badkey now))
                `shouldBe` Just (algorithmName HMAC_SHA256)

        it "carries no MAC when the key is not one we hold" $ do
            faultError badkey `shouldBe` BADKEY
            (Opaque.toByteString . tsig_mac <$> tsigOf (refusal badkey now))
                `shouldBe` Just ""

        it "carries no MAC when the MAC did not check out" $ do
            faultError badsig `shouldBe` BADSIG
            (Opaque.toByteString . tsig_mac <$> tsigOf (refusal badsig now))
                `shouldBe` Just ""

        it "keeps the identifier of the answer it goes on" $
            (tsig_original_id <$> tsigOf (refusal badkey now))
                `shouldBe` Just (identifier plain)

        it "says which error it was, where the far end can read it" $ do
            tsigReported (decoded $ refusal badkey now) `shouldBe` Just BADKEY
            tsigReported (decoded $ refusal badsig now) `shouldBe` Just BADSIG
            tsigReported (decoded $ refusal badtime server) `shouldBe` Just BADTIME

        it "is not mistaken for one on an answer which is not a refusal" $
            tsigReported (decoded $ signed Nothing plain) `shouldBe` Nothing

        -- Sec 5.2.3: signed with the same key, at the time the client
        -- gave, so that the client can check it without the clocks
        -- getting in the way a second time.
        it "signs a complaint about the clocks, as the client's clock has it" $ do
            faultError badtime `shouldBe` BADTIME
            let bs = refusal badtime server
            verifyTSIG held now (Just $ macOf late) bs (decoded bs)
                `shouldBe` TSIGOk (macOf bs)

        it "gives our own time in the other data of one" $ do
            let Just rd = tsigOf $ refusal badtime server
            tsig_time_signed rd `shouldBe` fromIntegral now
            tsig_fudge rd `shouldBe` defaultFudge
            Opaque.length (tsig_other rd) `shouldBe` 6
            sixOctetsOf (tsig_other rd) `shouldBe` toInteger server

----------------------------------------------------------------

now :: EpochTime
now = 1700000000

plain :: DNSMessage
plain =
    defaultQuery
        { identifier = 0xbeef
        , question = Question "example.jp." AXFR IN
        }

held :: Domain -> Maybe TSIGKey
held n
    | n == tsigKeyName key = Just key
    | otherwise = Nothing

stranger :: TSIGKey
stranger = key{tsigKeyName = "nobody.example.jp."}

flipBit :: Int -> BS.ByteString -> BS.ByteString
flipBit i bs =
    BS.concat [BS.take i bs, BS.singleton (BS.index bs i `xor` 1), BS.drop (i + 1) bs]

withRR :: ResourceRecord -> DNSMessage -> DNSMessage
withRR rr m = m{additional = additional m ++ [rr]}

signedWith :: TSIGKey -> Maybe Opaque -> DNSMessage -> BS.ByteString
signedWith k mreq m = encode $ withRR rr m
  where
    (rr, _) = signTSIG k now defaultFudge mreq (encode m)

signed :: Maybe Opaque -> DNSMessage -> BS.ByteString
signed = signedWith key

decoded :: BS.ByteString -> DNSMessage
decoded bs = case decode bs of
    Right m -> m
    Left e -> error $ show e

tsigOf :: BS.ByteString -> Maybe RD_TSIG
tsigOf bs = case reverse $ additional $ decoded bs of
    rr : _ -> fromRData $ rdata rr
    _ -> Nothing

macOf :: BS.ByteString -> Opaque
macOf bs = maybe (error "no TSIG") tsig_mac $ tsigOf bs

lastRR :: BS.ByteString -> ResourceRecord
lastRR bs = case reverse $ additional $ decoded bs of
    rr : _ -> rr
    _ -> error "no record"

-- | What was wrong, without the record and the key it was wrong on.
failure :: TSIGResult -> Maybe TSIGError
failure (TSIGFailed f) = Just $ faultError f
failure _ = Nothing

faultOf :: TSIGResult -> TSIGFault
faultOf (TSIGFailed f) = f
faultOf r = error $ "not a failure: " ++ show r

-- | The refusal a server puts together out of what it found wrong.
refusal :: TSIGFault -> EpochTime -> BS.ByteString
refusal fault t = encode $ withRR (errorTSIG fault t $ encode notauth) notauth

notauth :: DNSMessage
notauth = plain{rcode = NotAuth}

sixOctetsOf :: Opaque -> Integer
sixOctetsOf = BS.foldl' (\a w -> a * 256 + toInteger w) 0 . Opaque.toByteString

----------------------------------------------------------------

key, other, key512 :: TSIGKey
key = TSIGKey "k.example.jp." HMAC_SHA256 "a secret"
other = key{tsigKeySecret = "another secret"}
key512 = key{tsigKeyAlgorithm = HMAC_SHA512}

digest :: BS.ByteString
digest = "the octets a MAC is taken over"

truncated :: Int -> Opaque
truncated n = Opaque.fromByteString $ BS.take n $ Opaque.toByteString $ tsigMAC key digest

macBytes :: TSIGAlgorithm -> BS.ByteString -> BS.ByteString -> BS.ByteString
macBytes alg secret msg = Opaque.toByteString $ tsigMAC (TSIGKey "k." alg secret) msg

mac :: TSIGAlgorithm -> BS.ByteString -> BS.ByteString -> String
mac alg secret msg = concatMap (pad . flip showHex "") $ BS.unpack $ macBytes alg secret msg
  where
    pad s = if length s == 1 then '0' : s else s

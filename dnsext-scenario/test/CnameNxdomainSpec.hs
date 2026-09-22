{-# LANGUAGE OverloadedStrings #-}

-- | What bowline does with an NXDOMAIN sent over the top of a CNAME.
--
--   @alias.example.@ is a CNAME for a name in another zone.  Its server
--   sends the CNAME and then says NXDOMAIN in the header of the same
--   message.  Both halves cannot be read the same way: the name plainly
--   is there, the CNAME having just been given for it.
--
--   RFC 8020 Sec 2 says where a chain of CNAMEs is involved the name
--   which does not exist is the last of the chain and not the name
--   asked about.  So the header is not about @alias.example.@; and it
--   is not usable about @www.elsewhere.@ either, this server having no
--   standing to say anything about a zone it was not delegated.  There
--   is nothing in it to believe, and the target does in fact exist.
--
--   A resolver which reads it as being about the name asked for has
--   written that name off, and RFC 8020 then writes off everything
--   below it too.  c.uecac.jp and cname.small-is-beautiful.jp both put
--   a response of this shape in front of a resolver and ask 「この
--   NXDOMAIN を信じてよい?」.
--
--   Nothing here is signed, on purpose: signatures would give a
--   resolver a second and quite different reason to turn the response
--   down, and what is being asked about is how it reads one.
--
--   See https://www.e-ontap.com/dns/samples.html
module CnameNxdomainSpec (spec) where

import DNS.Types
import Test.Hspec

import Harness

spec :: Spec
spec = aroundAll (withScenario "cname-nxdomain") $
    describe "an NXDOMAIN sent over the top of a CNAME" $ do
        -- The CNAME is followed and the name it points at is found.
        it "follows the CNAME rather than believing the header" $ \sc -> do
            a <- ask sc "alias.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf CNAME a `shouldBe` [rd_cname "www.elsewhere."]
            rdataOf A a `shouldBe` [rd_a "198.51.100.1"]

        -- Which means it went and asked the zone which owns the target,
        -- rather than taking the word of a server with no standing to
        -- give it.
        it "asks the zone the target is in" $ \sc -> do
            _ <- ask sc "alias.example." A
            qs <- asked sc ThePrimary
            qs `shouldSatisfy` elem ("www.elsewhere.", A)

        -- And the name was not written off: asked for what it actually
        -- holds, bowline says it holds it.  A resolver which had taken
        -- the NXDOMAIN for an answer about this name would deny it here
        -- and deny everything below it as well.
        it "has not written the name off" $ \sc -> do
            a <- ask sc "alias.example." CNAME
            answerRcode a `shouldBe` NoErr
            rdataOf CNAME a `shouldBe` [rd_cname "www.elsewhere."]

        -- The rest of the zone is untouched, the fault being one name
        -- wide.
        it "is still the same zone everywhere else" $ \sc -> do
            a <- ask sc "www.example." A
            answerRcode a `shouldBe` NoErr
            rdataOf A a `shouldBe` [rd_a "192.0.2.1"]

-- | What the answer says of one type.
rdataOf :: TYPE -> Answer -> [RData]
rdataOf typ a = [rdata rr | rr <- answerRRs a, rrtype rr == typ]

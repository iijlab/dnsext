module QuerySpec where

import Test.Hspec

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import qualified DNS.RRCache as Cache
import qualified DNS.SEC as DNS
import DNS.Types (Question (..), TYPE (A, AAAA, CNAME, MX, NS, PTR, SOA))
import qualified DNS.Types as DNS
import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

import qualified DNS.Log as Log

import DNS.Do53.Client (FlagOp (..), cdFlag)
import DNS.Iterative.Internal (
    Delegation (..),
    Env (..),
    foldResponseIterative',
    newEmptyEnv,
    newTestCache,
    queryParamIN,
    refreshRoot,
    rootHint,
    rootPriming,
    rrsetValid,
    runDNSQuery,
 )
import qualified DNS.Iterative.Internal as I

data AnswerResult
    = Empty DNS.RCODE
    | NotEmpty DNS.RCODE
    | Failed
    deriving (Eq, Show)

data VerifyResult
    = Verified
    | NotVerified
    deriving (Eq, Show)

data VAnswerResult
    = VEmpty DNS.RCODE
    | VNotEmpty DNS.RCODE VerifyResult
    | VFailed
    | VCached
    deriving (Eq, Show)

newTestEnv :: Bool -> Log.PutLines IO -> IO Env
newTestEnv disableV6NS putLines = do
    env <- newEmptyEnv
    (getCache, insert) <- newTestCache (currentSeconds_ env) $ 2 * 1024 * 1024
    pure env{disableV6NS_ = disableV6NS, logLines_ = putLines, insert_ = insert, getCache_ = getCache, timeout_ = timeout 3000000}

spec :: Spec
spec = do
    let getEnvBool n = runIO $ maybe False ((== "1") . take 1) <$> lookupEnv n
    disableV6NS <- getEnvBool "DISABLE_V6_NS"
    debug <- getEnvBool "QTEST_DEBUG"
    runIO $ DNS.runInitIO DNS.addResourceDataForDNSSEC
    let debugLog = do
            (logger, _, putLines, flush) <- Log.new Log.Stdout Log.DEBUG
            void $ forkIO logger -- fixme
            pure (\lv c xs -> putLines lv c [show lv ++ ": " ++ x | x <- xs], flush)
        quiet = (\_ _ _ -> pure (), pure ())
    (putLines, flush) <- if debug then runIO debugLog else pure quiet
    envSpec
    cacheStateSpec disableV6NS putLines
    querySpec disableV6NS putLines
    runIO flush

envSpec :: Spec
envSpec = describe "env" $ do
    it "rootHint" $ do
        let sp p = case p of Delegation{} -> True -- check not error
        rootHint `shouldSatisfy` sp

cacheStateSpec :: Bool -> Log.PutLines IO -> Spec
cacheStateSpec disableV6NS putLines = describe "cache-state" $ do
    let getResolveCache n ty = do
            cxt <- newTestEnv disableV6NS putLines
            eresult <- fmap snd <$> I.runResolve cxt (Question (fromString n) ty DNS.IN) mempty
            threadDelay $ 1 * 1000 * 1000
            let convert xs =
                    [ ((dom, typ), (crs, rank))
                    | (Cache.Question dom typ _, (_, Cache.Val crs rank)) <- xs
                    ]
            (,) eresult . convert . Cache.dump <$> getCache_ cxt
        clookup cs n typ = lookup (fromString n, typ) cs
        check cs n typ = lookup (fromString n, typ) cs
        nodata = maybe False (\(crs, _rank) -> Cache.hitCases1 (const True) (const False) crs)

    it "answer - a" $ do
        (_, cs) <- getResolveCache "iij.ad.jp." A
        fmap snd (clookup cs "iij.ad.jp." A) `shouldSatisfy` (>= Just Cache.RankAnswer)

    it "not zone - nodata ns" $ do
        (_, cs) <- getResolveCache "iij.ad.jp." A
        check cs "ad.jp." NS `shouldSatisfy` nodata

    it "not zone - soa of nodata ns" $ do
        (_, cs) <- getResolveCache "iij.ad.jp." A
        check cs "jp." SOA `shouldSatisfy` isJust

    it "shared child zone - ns" $ do
        (_, cs) <- getResolveCache "1.1.1.1.in-addr.arpa." PTR
        check cs "arpa." NS `shouldSatisfy` isNothing

    it "shared child zone - child apex a" $ do
        (_, cs) <- getResolveCache "www.cloudflare.com." MX
        check cs "www.cloudflare.com." MX `shouldSatisfy` isJust

querySpec :: Bool -> Log.PutLines IO -> Spec
querySpec disableV6NS putLines = describe "query" $ do
    let getCXT = newTestEnv disableV6NS putLines
    cxt <- runIO getCXT
    cxt4 <- runIO $ newTestEnv True putLines
    let runIterative_ ns n = I.runIterative cxt ns (fromString n) mempty
        runExactCXT cxt_ n ty = I.runResolveExact cxt_ (fromString n) ty mempty
        runJust = runExactCXT cxt
        runResolveCXT_ cxt_ n ty flags = fmap snd <$> I.runResolve cxt_ (Question (fromString n) ty DNS.IN) flags
        runResolveCXT cxt_ n ty = fmap snd <$> I.runResolve cxt_ (Question (fromString n) ty DNS.IN) mempty
        runResolve_ = runResolveCXT cxt
        getReply n0 ty ident = do
            let n = fromString n0
                q = Question n ty DNS.IN
            foldResponseIterative' Left (\_ -> Right) cxt ident q mempty

    let failLeft p = either (fail . ((p ++ ": ") ++) . show) pure
        printQueryError :: Show e => Either e a -> IO ()
        printQueryError = either (putStrLn . ("    QueryError: " ++) . show) (const $ pure ())
        _pprResult (msg, (ans, auth)) =
            unlines $
                ("rcode: " ++ show (DNS.rcode msg))
                    : "answer:"
                    : map (("  " ++) . show) ans
                    ++ "authority:"
                    : map (("  " ++) . show) auth

    let checkAnswer msg
            | null (DNS.answer msg) = Empty rcode
            | otherwise = NotEmpty rcode
          where
            rcode = DNS.rcode msg
        checkResult = either (const Failed) (checkAnswer . fst)

    let verified rrsets
            | all rrsetValid rrsets = Verified
            | otherwise = NotVerified
        checkVAnswer (msg, vans, _)
            | null vans = VEmpty rcode
            | otherwise = VNotEmpty rcode (verified vans)
          where
            rcode = DNS.rcode msg
        cachedVAnswer (_rcode, _rrs, _) = VCached
        checkVResult = either (const VFailed) (either cachedVAnswer checkVAnswer)

    it "root-priming" $ do
        result <- runDNSQuery rootPriming cxt $ queryParamIN (fromString ".") NS mempty
        printQueryError result
        either (expectationFailure . show) (`shouldSatisfy` isRight) result

    root <- runIO $ do
        icxt <- newTestEnv disableV6NS (\_ _ _ -> pure ())
        failLeft "refresh-root error" =<< runDNSQuery refreshRoot icxt (queryParamIN (fromString ".") NS mempty)

    it "iterative" $ do
        result <- runIterative_ root "iij.ad.jp."
        printQueryError result
        result `shouldSatisfy` isRight

    it "iterative - many" $ do
        result <- runIterative_ root "media-router-aol1.prod.g03.yahoodns.net."
        printQueryError result
        result `shouldSatisfy` isRight

    it "resolve-just - ns" $ do
        result <- runJust "iij.ad.jp." NS
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - a" $ do
        result <- runJust "iij.ad.jp." A
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - aaaa" $ do
        result <- runJust "iij.ad.jp." AAAA
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - mx" $ do
        result <- runJust "iij.ad.jp." MX
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - cname" $ do
        result <- runJust "www.aol.com." CNAME
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - ptr" $ do
        result <- runJust "1.1.1.1.in-addr.arpa." PTR
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - nx NSEC" $ do
        result <- runJust "does-not-exist.dns-oarc.net." A
        checkResult result `shouldBe` Empty DNS.NXDomain

    it "resolve-just - nodata NSEC" $ do
        result <- runJust "mail.dns-oarc.net." NS
        checkResult result `shouldBe` Empty DNS.NoErr

    it "resolve-just - nx NSEC3" $ do
        result <- runJust "does-not-exist.iij.ad.jp." A
        checkResult result `shouldBe` Empty DNS.NXDomain

    it "resolve-just - nodata NSEC3" $ do
        result <- runJust "www.iij.ad.jp." NS
        checkResult result `shouldBe` Empty DNS.NoErr

    it "resolve-just - nx on iterative" $ do
        result <- runJust "media-router-aol1.prod.media.yahoo.com." CNAME
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve-just - delegation with aa" $ do
        -- `dig -4 @ns1.alibabadns.com. danuoyi.alicdn.com. A` has delegation authority section with aa flag
        result <- runExactCXT cxt4 "sc02.alicdn.com.danuoyi.alicdn.com." A
        printQueryError result
        checkResult result `shouldBe` NotEmpty DNS.NoErr

    it "resolve - cname" $ do
        result <- getCXT >>= \cxtI -> runResolveCXT cxtI "www.aol.com." CNAME
        printQueryError result
        checkVResult result `shouldBe` VNotEmpty DNS.NoErr NotVerified

    it "resolve - a via cname" $ do
        result <- runResolve_ "clients4.google.com." A
        printQueryError result
        isRight result `shouldBe` True

    it "resolve - a with DNSSEC_OK" $ do
        result <- getCXT >>= \cxtI -> runResolveCXT cxtI "iij.ad.jp." A
        printQueryError result
        checkVResult result `shouldBe` VNotEmpty DNS.NoErr Verified

    it "resolve - a with check-disabled" $ do
        result <- getCXT >>= \cxtI -> runResolveCXT_ cxtI "brokendnssec.net." A (cdFlag FlagSet)
        printQueryError result
        checkVResult result `shouldBe` VNotEmpty DNS.NoErr NotVerified

    it "resolve - nodata with check-disabled" $ do
        result <- getCXT >>= \cxtI -> runResolveCXT_ cxtI "fail.dnssec.jp." A (cdFlag FlagSet)
        printQueryError result
        checkVResult result `shouldBe` VEmpty DNS.NoErr

    let cnamePointNX = "cnnx.ts.reasonings.cc."
    it "get-reply - nx via cname - cname" $ do
        rcname <- getReply cnamePointNX CNAME 0
        either (const Failed) checkAnswer rcname `shouldBe` NotEmpty DNS.NoErr
    it "get-reply - nx via cname - nx" $ do
        result <- getReply cnamePointNX A 0
        either (const DNS.ServFail) DNS.rcode result `shouldBe` DNS.NXDomain

    it "get-reply - a accumulated via cname" $ do
        result <- getReply "media-router-aol1.prod.media.yahoo.com." A 0
        either (const 0) (length . DNS.answer) result `shouldSatisfy` (> 1)

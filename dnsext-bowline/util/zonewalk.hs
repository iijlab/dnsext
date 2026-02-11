{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import DNS.Do53.Client
import DNS.Do53.Internal
import DNS.SEC
import DNS.Types
import qualified Data.ByteString.Short as Short
import Data.IP
import Data.Maybe
import System.Environment
import System.Exit

main :: IO ()
main = do
    runInitIO addResourceDataForDNSSEC
    [dom0] <- getArgs
    let dom = fromRepresentation dom0
    addr <- withLookupConf defaultLookupConf $ \env -> do
        ens <- lookupNS env dom
        case ens of
            Right (ns : _) -> do
                ea <- lookupA env $ ns_domain ns
                case ea of
                    Right (a : _) -> return $ IPv4 $ a_ipv4 a
                    _ -> die "No A RRs for NS"
            _ -> die "No NS RRs"
    withLookupConf (conf addr) $ \env -> walk env dom
  where
    conf addr =
        defaultLookupConf
            { lconfQueryControls = mconcat [rdFlag FlagClear, doFlag FlagSet]
            , lconfSeeds = SeedsAddr addr
            }

walk :: LookupEnv -> Domain -> IO ()
walk env dom0 = loop dom0'
  where
    zero :: Short.ShortByteString
    zero = "\x00"
    dom0' = fromWireLabels (zero : toWireLabels dom0)
    loop dom = do
        bes <- getNSEC env dom
        case filter (\(beg, end) -> beg < dom && dom < end) bes of
            (beg, end) : _ -> do
                printRange beg end
                when (end /= dom) $ do
                    loop $ modify end
            _ -> checkNSEC3 env dom
    modify dom = case toWireLabels dom of
        [] -> fromWireLabels [zero]
        l : ls -> fromWireLabels (l <> zero : ls)

getNSEC :: LookupEnv -> Domain -> IO [(Domain, Domain)]
getNSEC env dom = do
    let q = Question dom NSEC IN
    ex <- lookupRaw env q
    case ex of
        Left e -> die $ show e
        Right reply -> return $ extractNSECs reply

checkNSEC3 :: LookupEnv -> Domain -> IO ()
checkNSEC3 env dom = do
    let q = Question dom NSEC3 IN
    ex <- lookupRaw env q
    case ex of
        Left e -> die $ show e
        Right reply -> case filter (\r -> rrtype r == NSEC3) $ authority $ replyDNSMessage reply of
            [] -> putStrLn "No NSECs and NSEC3s"
            _ -> putStrLn "NSEC3 only"

extractNSECs :: Reply -> [(Domain, Domain)]
extractNSECs reply = catMaybes $ map toNSEC $ authority $ replyDNSMessage reply

toNSEC :: ResourceRecord -> Maybe (Domain, Domain)
toNSEC rr = case fromRData (rdata rr) :: Maybe RD_NSEC of
    Nothing -> Nothing
    Just nsec -> Just (rrname rr, nsec_next_domain nsec)

printRange :: Domain -> Domain -> IO ()
printRange beg end =
    putStrLn $ toRepresentation beg ++ " --- " ++ toRepresentation end

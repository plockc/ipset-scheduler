module Main where

import Lib
import Data.Either (fromRight, fromLeft, isLeft)
import Data.Typeable
import Data.Map (Map, toList)
import Data.IP (IPv4)
import Data.ByteString.Char8 (pack)
import qualified Data.Yaml as Yaml (decodeFileThrow)
import Network.DNS.Lookup (lookupA)
import Network.DNS.Resolver
import Control.Monad
import System.Process (readProcessWithExitCode)
import System.Exit


ipsForHost :: ResolvSeed -> String -> IO [IPv4]
ipsForHost rs host = do
    ipsResult <- withResolver rs $ \resolver -> lookupA resolver $ pack host
    -- empty list is the default in case of any Left error
    when (isLeft ipsResult) (putStrLn $ show ipsResult)
    -- use non-error Right result from Either else use default of empty list
    return $ fromRight [] ipsResult

cmdAddToIPSet :: String -> IPv4 -> IO ()
cmdAddToIPSet ipSetName ip = do
    (code, stdout, stderr) <- readProcessWithExitCode "echo" ["WOULD RUN: ipset", "add", ipSetName, show ip] ""
    if (code == ExitSuccess) then 
        putStrLn stdout
    else
        putStrLn $ "Failed(" ++ show code ++ ") " ++ stderr
    
setIPSet :: String -> [IPv4] -> IO ()
setIPSet ipSetName ipList = do
    mapM_ (cmdAddToIPSet ipSetName) ipList

main :: IO ()
main = do
    ipsetsData <- Yaml.decodeFileThrow "./test/ipsets.yaml" :: IO (Map String [String])
    rs <- makeResolvSeed defaultResolvConf
    -- given a String list of hostnames, convert to IO of List of Lists of IPs
    -- the fmap allows me to use a pure function concat (flattens list of lists) in the IO Monad
    -- the (ipsResult rs) is a partially applied function that can take in a host from hostlist
    -- [[Char]] -> IO [IPv4]
    let lookupHosts = \hostList -> fmap concat $ mapM (ipsForHost rs) hostList
    -- list comprehension to lookup the list of hosts for each ipset
    -- [([Char],(IO [IPv4]))]
    ipSetNamesToIPs <- return [ (k, lookupHosts v) | (k, v) <- toList ipsetsData ]
    -- setIPSet gets partially applied and <$!> allows the function to unwrap the IO Parameter since
    -- it returns the IO monad
    -- 
    -- seems like we've ended up wrapping the IO when we shouldn't which required the join
    let s3 = \(ipSetName, ioIpList) -> join $ (setIPSet ipSetName) <$!> ioIpList
    mapM_ s3 ipSetNamesToIPs

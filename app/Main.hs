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
    
-- Update the named IP set with the list of IP addresses
setIPset :: String -> IO [IPv4] -> IO ()
setIPset ipSetName ipList = do
    -- lift show so that can work on IO wrapped list
    -- fmap show across the ipList to convert to IO [String]
    printedList <- fmap (liftM show) ipList
    putStr $ ipSetName ++ "\n------------\n"
    -- map for IO to print line with each element of the printedList
    mapM_ putStrLn printedList
    print ""

setIPSet2 :: String -> [IPv4] -> IO ()
setIPSet2 ipSetName ipList = do
    let addCmd = (cmdAddToIPSet ipSetName)
    mapM_ addCmd ipList

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
    -- each element in ipSetNamesToIPs is a tuple of args to setIpSet, unpack so can call setIpSet for each
    mapM_ (\ipSetNameToIPs -> setIPset (fst ipSetNameToIPs) (snd ipSetNameToIPs) ) ipSetNamesToIPs
    -- setIPSet gets partially applied and <$!> allows the function to unwrap the IO Parameter since
    -- it returns the IO monad
    -- 
    let s3 = (\(ipSetName, ioIpList) -> join $ (setIPSet2 ipSetName) <$!> ioIpList) :: (String, IO [IPv4]) -> IO ()
    print $ typeOf s3
    mapM_ s3 ipSetNamesToIPs

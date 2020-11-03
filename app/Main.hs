{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Debug.Trace (trace)
import Data.Either (Either(Left), Either(Right), fromRight, fromLeft, isLeft)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Data.List (foldr)
import qualified Data.Text as Text (Text, splitOn, pack, unpack)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, empty, toList, member, unions, filter)
import Data.IP (IPv4)
import Data.ByteString.Char8 (pack)
import Data.Time.Calendar (Day, DayOfWeek, dayOfWeek, toGregorian)
import Data.Time.LocalTime
import Data.Aeson.Types (Value, Parser, Value(String))
import qualified Data.Yaml as Yaml (decodeFileThrow, FromJSON, parseJSON, withText)
import Network.DNS.Lookup (lookupA)
import Network.DNS.Resolver
import Control.Monad
import System.Process (readProcessWithExitCode)
import System.Exit
import Safe (readEitherSafe)
import GHC.Generics as Generics (Generic)

type ServiceGroupName = String
type Host = String

data Action = Block | Allow deriving(Generics.Generic, Eq, Show)
instance Yaml.FromJSON Action

data ActionsForServiceGroups = ActionsForServiceGroups (Map ServiceGroupName Action) deriving(Generics.Generic, Eq, Show)
instance Yaml.FromJSON ActionsForServiceGroups

data HourMinute = HourMinute {
    hour :: Integer,
    minute :: Integer
} deriving (Generics.Generic, Eq, Show, Ord)

parseHourMinute :: Value -> Parser HourMinute
parseHourMinute (String time) = do
    case Text.splitOn (Text.pack ":") time of
        [hourText, minuteText] -> do
            -- wrap the -1 so it's not part of a binary function
            let hour = fromRight (-1) $ readEitherSafe (Text.unpack hourText)
                minute = fromRight (-1) $ readEitherSafe (Text.unpack minuteText)
                in return (HourMinute hour minute)
        _ -> fail $ "whoops, not parseable as hour:minute in 24hr notation" ++ Text.unpack time
parseHourMinute _ = fail "Was not a String"

instance Yaml.FromJSON HourMinute where
    parseJSON = parseHourMinute

data RuleConfig = RuleConfig {
    action :: Action,
    start :: HourMinute,
    end :: HourMinute,
    services :: Maybe [ServiceGroupName]
} deriving (Generics.Generic, Eq, Show)
instance Yaml.FromJSON RuleConfig

data CalendarConfig = CalendarConfig {
    rules :: String,
    days_of_week :: Maybe [DayOfWeek]
} deriving (Generics.Generic, Eq, Show)
instance Yaml.FromJSON CalendarConfig

data ScheduleConfig = ScheduleConfig {
    serviceGroups :: Map ServiceGroupName [Host],
    clients :: Map String String,
    client_sets :: [Map String [String]],
    schedule :: Map String [RuleConfig],
    calendar :: [CalendarConfig]
} deriving (Generics.Generic, Eq, Show)
instance Yaml.FromJSON ScheduleConfig

ipsForHost :: Host -> IO [IPv4]
ipsForHost host = do
    rs <- makeResolvSeed defaultResolvConf
    ipsResult <- withResolver rs $ \resolver -> lookupA resolver $ pack host
    -- empty list is the default in case of any Left error
    when (isLeft ipsResult) (putStrLn $ "Failed Lookup for " ++ host ++ ": " ++ show ipsResult)
    -- use non-error Right result from Either else use default of empty list
    return $ fromRight [] ipsResult

cmdAddToIPSet :: String -> IPv4 -> IO ()
cmdAddToIPSet ipSetName ip = do
    (code, stdout, stderr) <- readProcessWithExitCode "echo" ["WOULD RUN: ipset", "-exist", "add", ipSetName, show ip] ""
    if (code == ExitSuccess) then 
        putStr stdout
    else
        putStrLn $ "Failed(" ++ show code ++ ") " ++ stderr
    
setIPSet :: String -> [IPv4] -> IO ()
setIPSet ipSetName ipList = do
    (code, stdout, stderr) <- readProcessWithExitCode "echo" ["WOULD RUN: ipset", "-exist", "create", ipSetName, "hash:ip", "timeout", "70"] ""
    if (code == ExitSuccess) then 
        putStr stdout
    else
        putStrLn $ "Failed creating ipset " ++ ipSetName ++ " (" ++ show code ++ ") " ++ stderr
    mapM_ (cmdAddToIPSet ipSetName) ipList

ruleSetsForDay :: [CalendarConfig] -> DayOfWeek -> [String]
-- if days of week are not specified, it is all days
ruleSetsForDay calendarConfigs dayOfWeek =
    [ rules | CalendarConfig rules maybe_days_of_week <- calendarConfigs,
        maybe True (elem dayOfWeek) maybe_days_of_week]

main :: IO ()
main = do
    scheduleConfig <- Yaml.decodeFileThrow "./test/schedule.yaml" :: IO ScheduleConfig
    
    -- gather time and date data
    ZonedTime (LocalTime localDay (TimeOfDay hour minute _)) _ <- getZonedTime
    let (year, month, dayOfMonth) = toGregorian localDay
        now = HourMinute (toInteger hour) (toInteger minute)
        weekDay = dayOfWeek localDay

        ruleSets = ruleSetsForDay (calendar scheduleConfig) weekDay :: [String]
        serviceGroupNames = Map.keys $ serviceGroups scheduleConfig

        rulesForDay = flip concatMap ruleSets $
            flip (Map.findWithDefault []) (schedule scheduleConfig)
        rulesForNow = Prelude.filter (\x -> start x <= now && end x >= now ) rulesForDay
        -- update the Nothings for services in the rules to be all services
        actionMap = Map.unions $
            flip map rulesForNow 
                (\r -> Map.fromList $
                    map (\s -> (s, action r)) $ fromMaybe serviceGroupNames $ services r)
        -- may not use this
        blocked = Map.keys $ Map.filter ((==) Block) actionMap

    -- there is a server ip set, there is also a set for each server for the blocked clients 
    -- there is a chain of all the blocks from clien to server
    -- clients are added or removed from the client sets based on block or allow
    -- there is one chain that hold all the blocks from each server set to client set combo
    
    putStrLn $ "All rules for today are " ++ show rulesForDay
    putStrLn $ "Active are " ++ show rulesForNow
    putStr $ "Today is " ++ show weekDay ++ " " ++ show hour ++ ":" ++ show minute 
    putStrLn $ " " ++ show year ++ "-" ++ show month ++ "-" ++ show dayOfMonth 
    putStrLn $ "Rule Sets to consider due to what day it is: " ++ show ruleSets
    putStrLn $ "Blocked: " ++ show blocked
 
    -----------------------------------------
 
    -- given a String list of hostnames, convert to IO of List of Lists of IPs
    -- the fmap allows me to use a pure function concat (flattens list of lists) in the IO Monad
    -- the (ipsResult rs) is a partially applied function that can take in a host from hostlist
    -- [[Char]] -> IO [IPv4]
    let lookupHosts = \hostList -> fmap concat $ mapM ipsForHost hostList
    -- list comprehension to lookup the list of hosts for each ipset
    -- [([Char],(IO [IPv4]))]
    ipSetNamesToIPs <- return [ (k, lookupHosts v) | (k, v) <- Map.toList $ serviceGroups scheduleConfig ]
    -- setIPSet gets partially applied and <$!> allows the function to unwrap the IO Parameter since
    -- it returns the IO monad
    -- 
    -- seems like we've ended up wrapping the IO when we shouldn't which required the join
    let s3 = \(ipSetName, ioIpList) -> join $ (setIPSet ipSetName) <$!> ioIpList
    mapM_ s3 ipSetNamesToIPs


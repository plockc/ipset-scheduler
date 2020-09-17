{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Debug.Trace (trace)
import Data.Either (Either(Left), Either(Right), fromRight, fromLeft, isLeft)
import Data.Typeable
import Data.List (foldr)
import qualified Data.Text as Text (Text, splitOn, pack, unpack)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, empty, toList, member)
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
    ipsResult <- withResolver rs $ \resolver -> lookupA resolver $ pack $ show host
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
    let weekDay = dayOfWeek localDay
        ruleSets = ruleSetsForDay (calendar scheduleConfig) weekDay :: [String]
        serviceGroupNames = Map.keys $ serviceGroups scheduleConfig
        now = HourMinute (toInteger hour) (toInteger minute)

        -- function that takes a rule set name and returns the Maybe rules for it
        fullSchedule = schedule scheduleConfig :: Map String [RuleConfig]
        rulesForSet = flip (Map.findWithDefault []) fullSchedule :: String -> [RuleConfig]
        rulesForDay = concatMap (rulesForSet) ruleSets :: [RuleConfig]
        rulesForNow = filter (\x -> start x <= now && end x >= now ) rulesForDay
        -- convert to (Maybe [ServiceGroupName], Action)
        -- convert Nothing to all ServiceGroupNames, and drop the Maybe
        -- convert to ActionsForServiceGroups (which is for each service) which gives final Rule
        -- then figure out how to apply the rules

    putStrLn $ "All rules for today are " ++ show rulesForDay
    putStrLn $ "Active are " ++ show rulesForNow
    putStrLn $ show year ++ " " ++ show month ++ " " ++ show dayOfMonth 
    putStrLn $ "Today is " ++ show weekDay ++ " " ++ show hour ++ ":" ++ show minute 
    putStrLn $ "Rule Sets to consider: " ++ show ruleSets
    putStrLn $ "All service Groups: " ++ show serviceGroupNames
 
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


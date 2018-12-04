module Day4
  (
  ) where


import Helpers
import Data.Fixed
import Data.List ( maximumBy, sortBy, groupBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord ( comparing )
import Data.Time ( LocalTime, readSTime, defaultTimeLocale, diffLocalTime, NominalDiffTime, todMin, localTimeOfDay )


type GuardId = Int
type Minutes = Int

data Sleep = Sleep { guardId, timeMinutes, duration :: !Int } deriving (Eq, Show)

data EntryType = BeginShift GuardId | FallsAsleep | WakesUp deriving Show

-- [1518-11-01 00:00] Guard #10 begins shift
parseRecord :: String -> (LocalTime, EntryType)
parseRecord input =
  case readSTime True defaultTimeLocale "[%Y-%m-%d %H:%M]" input of
    [(time, activity)] -> (time, toEntryType activity)

-- 'Guard #10 begins shift' to EntryType 
toEntryType :: String -> EntryType
toEntryType activity =
  case words activity of
    ["wakes", "up"]                               -> WakesUp
    ["falls", "asleep"]                           -> FallsAsleep
    ["Guard", '#':guardNumber, "begins", "shift"] -> BeginShift (read guardNumber)
    _  -> error ("Incorrect entry type" ++ activity)

prepareInput :: [String] -> [(LocalTime, EntryType)]
prepareInput = sortBy (comparing fst) . map parseRecord

testData :: [String]
testData =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  ]

t1 = fst $ parseRecord "[1518-11-01 00:00] Guard #10 begins shift"
t2 = fst $ parseRecord "[1518-11-01 00:05] falls asleep"

toSleepTimes :: [(LocalTime, EntryType)] -> [Sleep] 
toSleepTimes = go (error "Missing begin record")
  where
    go _ ((_, BeginShift guardId) : xs) = go guardId xs
    go guardId ((t1, FallsAsleep) : (t2, WakesUp) : xs) =
      [Sleep guardId (getMinute t1) (calcMinutesDiff t1 t2)] ++ go guardId xs
    go _ _ = []

calcMinutesDiff :: LocalTime -> LocalTime -> Minutes
calcMinutesDiff t1 t2 = abs . minutes  $ diffLocalTime t1 t2

minutes :: NominalDiffTime -> Minutes
minutes n = floor . realToFrac $ n / 60

part1 :: [Sleep] ->  Int
part1 sleepMins = biggestSleeper
  where
    biggestSleeper = biggestKey . aggregateOccurrences . toIdDuration $ sleepMins
    timeOfLonger   = timeMinutes (maximumBy (comparing duration) $ filter (\i -> guardId i == biggestSleeper) sleepMins)

sleptAccidents :: [String] -> [Sleep]
sleptAccidents = toSleepTimes . prepareInput
  
biggestKey :: Ord a => Map k a -> k
biggestKey = fst . maximumBy (comparing snd) . Map.toList

getMinute :: LocalTime -> Int
getMinute = todMin . localTimeOfDay

toIdDuration :: [Sleep] -> [(Int, Int)]
toIdDuration = map (\s -> (guardId s, duration s))

timeForMaxDuration :: [Sleep] -> GuardId -> Int
timeForMaxDuration xs gId = timeMinutes (maximumBy (comparing duration) $ filter (\i -> guardId i == gId) xs)

sleptMinute :: [String] -> GuardId -> Int
sleptMinute xs gId = (timeForMaxDuration (sleptAccidents xs) gId)

biggestSleeper :: [String] -> Int
biggestSleeper = biggestKey . aggregateOccurrences . toIdDuration . sleptAccidents

runTest :: () -> Int
runTest _ = part1 . sleptAccidents $ testData

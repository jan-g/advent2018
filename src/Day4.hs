module Day4 where

import System.Environment

import Lib

import Data.Counter (Counter, empty, update, count, union, updateWith)
import qualified Data.Map.Strict (filter, size)
import Data.Map.Strict ((!), toList, foldlWithKey, filterWithKey)
import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Function (on)
import Data.List


{-
You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab. You need
to sneak inside and fix the issues with the suit, but there's a guard stationed outside the lab, so this is as close as
you can safely get.

As you search the closet for anything that might help, you discover that you're not the first person to want to sneak
in. Covering the walls, someone has spent an hour starting every midnight for the past few months secretly observing
this guard post! They've been writing down the ID of the one guard on duty that night - the Elves seem to have decided
that one guard was enough for the overnight shift - as well as when they fall asleep or wake up while at their post
(your puzzle input).

For example, consider the following records, which have already been organized into chronological order:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up

Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always the one
whose shift most recently started. Because all asleep/awake times are during the midnight hour (00:00 - 00:59), only the
minute portion (00 - 59) is relevant for those events.

Visually, these records show that the guards are asleep at these times:

Date   ID   Minute
            000000000011111111112222222222333333333344444444445555555555
            012345678901234567890123456789012345678901234567890123456789
11-01  #10  .....####################.....#########################.....
11-02  #99  ........................................##########..........
11-03  #10  ........................#####...............................
11-04  #99  ....................................##########..............
11-05  #99  .............................................##########.....

The columns are Date, which shows the month-day portion of the relevant day; ID, which shows the guard on duty that day;
and Minute, which shows the minutes during which the guard was asleep within the midnight hour. (The Minute column's
header shows the minute's ten's digit in the first row and the one's digit in the second row.) Awake is shown as ., and
asleep is shown as #.

Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up. For
example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.

If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard into
working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best
guard/minute combination.

Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?

In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only
slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other
minute the guard was asleep was only seen on one day).

While this example listed the entries in chronological order, your entries are in the order you found them. You'll need
to organize them before they can be analyzed.

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be
10 * 24 = 240.)
-}

day4 ls = do
  {- let ls = ["#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2"] -}
  putStrLn ("number of lines, raw = " ++ (show $ length ls))
  let rs = catMaybes $ map parseObservation ls
      observations = sortBy (compare `on` ts) rs
      {- (id, minute): count -}
      totalTimes = extractTimes $ foldl (processObservation) (emptyState) observations :: Counter (Integer, Integer) Integer
      summariseTimes = foldlWithKey
                         (\ acc (id,m) count -> updateWith id count acc)
                         (Data.Counter.empty :: Counter Integer Integer)
                         totalTimes
      (biggestId, biggestT) = head $ sortBy (\(id, t) (id', t') -> compare t' t) $ toList summariseTimes
  putStrLn ("runmber of lines, parsed = " ++ (show $ length rs))
  putStrLn ((show biggestId) ++ " was the id with total time " ++ (show biggestT))
  {- find the maximal minute for that guard -}
  let times = filterWithKey (\(id, m) c -> id == biggestId) totalTimes
      popularMinutes = sortBy (\(_, c) (_, c') -> compare c' c) $ toList times
      ((_, biggestM), _) = head $ popularMinutes
  putStrLn (show times)
  putStrLn (show popularMinutes)
  putStrLn ((show biggestM) ++ " was the most popular sleeping minute")
  putStrLn $ show $ biggestId * biggestM


data Time = Time { year :: Integer
                 , month :: Integer
                 , day :: Integer
                 , hour :: Integer
                 , minute :: Integer
                 }
            deriving (Show, Eq, Ord)

data Event = ShiftStart Integer
           | Wakes
           | Sleeps
            deriving (Show)

data Observation = Observation { ts :: Time
                               , event :: Event
                               }
            deriving (Show)


{-
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
-}
parseObservation s =
  case readP_to_S observationParser s of
    [] -> Nothing
    [(e, "")] -> Just e

timeStampParser :: ReadP Time
timeStampParser = do
  char '['
  year <- intParser
  char '-'
  month <- intParser
  char '-'
  day <- intParser
  char ' '
  hour <- intParser
  char ':'
  minute <- intParser
  char ']'
  return Time { year = year
              , month = month
              , day = day
              , hour = hour
              , minute = minute
              }

observationParser :: ReadP Observation
observationParser = do
  ts <- timeStampParser
  char ' '
  event <- eventParser
  eof
  return Observation { ts = ts, event = event }

eventParser :: ReadP Event
eventParser = do
  shiftParser <++ wakeParser <++ sleepParser

shiftParser = do
  string "Guard #"
  id <- intParser
  string " begins shift"
  return $ ShiftStart id

wakeParser = do
  string "wakes up"
  return Wakes

sleepParser = do
  string "falls asleep"
  return Sleeps


data State = Empty (Counter (Integer, Integer) Integer)
           | Awake (Counter (Integer, Integer) Integer) Integer Time
           | Asleep (Counter (Integer, Integer) Integer) Integer Time

emptyState = Empty $ Data.Counter.empty

extractTimes (Empty times) = times
extractTimes (Awake times _ _) = times
extractTimes (Asleep times _ _) = times

processObservation :: State -> Observation -> State

processObservation (Empty times) (Observation {ts=ts, event=ShiftStart id}) = Awake times id ts
processObservation (Awake times _ _) (Observation {ts=ts, event=ShiftStart id}) = Awake times id ts
processObservation (Asleep times _ _) (Observation {ts=ts, event=ShiftStart id}) = error "New shift starts with old shift sleeping"

processObservation (Empty times) (Observation {ts=ts, event=Wakes}) = error "We don't know who wakes"
processObservation (Awake times id _) (Observation {ts=ts, event=Wakes}) = Awake times id ts
processObservation (Asleep times id ts) (Observation {ts=ts', event=Wakes}) =
  {- Update the time slept by this individual -}
  let sleptMap = Data.Counter.count [(id, m) | m <- [minute ts .. (minute ts') - 1]]
      times' = Data.Counter.union times sleptMap
  in  Awake times' id ts'

processObservation (Empty times) (Observation {ts=ts, event=Sleeps}) = error "we don't know who sleeps"
processObservation (Awake times id _) (Observation {ts=ts, event=Sleeps}) = Asleep times id ts
processObservation (Asleep times id ts) (Observation {ts=_, event=Sleeps}) = Asleep times id ts

timeDiff t1 t2 = (minute t1) - (minute t2)



{-
Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?

In the example above, Guard #99 spent minute 45 asleep more than any other guard or minute - three times in total. (In
all other cases, any guard spent any minute asleep at most twice.)

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 99
* 45 = 4455.)
-}

day4b ls = do
  {- let ls = ["#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2"] -}
  putStrLn ("number of lines, raw = " ++ (show $ length ls))
  let rs = catMaybes $ map parseObservation ls
      observations = sortBy (compare `on` ts) rs
      {- (id, minute): count -}
      totalTimes = extractTimes $ foldl (processObservation) (emptyState) observations :: Counter (Integer, Integer) Integer
      summariseTimes = foldlWithKey
                         (\ acc (id,m) count -> updateWith id count acc)
                         (Data.Counter.empty :: Counter Integer Integer)
                         totalTimes
      (biggestId, biggestT) = head $ sortBy (\(id, t) (id', t') -> compare t' t) $ toList summariseTimes
  putStrLn ("runmber of lines, parsed = " ++ (show $ length rs))
  putStrLn ((show biggestId) ++ " was the id with total time " ++ (show biggestT))
  {- find the maximal minute for that guard -}
  let times = filterWithKey (\(id, m) c -> id == biggestId) totalTimes
      popularMinutes = sortBy (\(_, c) (_, c') -> compare c' c) $ toList times
      ((_, biggestM), _) = head $ popularMinutes
  putStrLn (show times)
  putStrLn (show popularMinutes)
  putStrLn ((show biggestM) ++ " was the most popular sleeping minute")
  putStrLn $ show $ biggestId * biggestM
  {- part two -}
  let popularMinutesOfAll = sortBy (\((id, m), c) ((id', m'), c') -> compare c' c) $ toList totalTimes
      ((biggestId', biggestT'), biggestC') = head popularMinutesOfAll
  putStrLn (show popularMinutesOfAll)
  putStrLn ("guard with most popular minute was " ++ (show biggestId')
            ++ " and minute was " ++ (show biggestT')
            ++ " product is " ++ (show $ biggestId' * biggestT'))


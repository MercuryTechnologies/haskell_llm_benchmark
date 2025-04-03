module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, dayOfWeek, DayOfWeek(..))

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth
              deriving (Eq, Show)

weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  case schedule of
    First  -> findDayInWeek 1 7 weekday year month
    Second -> findDayInWeek 8 14 weekday year month
    Third  -> findDayInWeek 15 21 weekday year month
    Fourth -> findDayInWeek 22 28 weekday year month
    Last   -> findLastDayInWeek year month weekday
    Teenth -> findDayInWeek 13 19 weekday year month

findDayInWeek :: Int -> Int -> Weekday -> Integer -> Int -> Day
findDayInWeek start end weekday year month =
  let dayInt = weekdayToInt weekday
      days = [start..end]
      possibleDays = map (\d -> fromGregorian year month d) days
      correctDays = filter (\d -> dayOfWeek d == intToDayOfWeek dayInt) possibleDays
  in  head correctDays

findLastDayInWeek :: Integer -> Int -> Weekday -> Day
findLastDayInWeek year month weekday =
  let dayInt = weekdayToInt weekday
      lastDayOfMonth = lastDayOfMonth' year month
      days = [lastDayOfMonth, lastDayOfMonth - 1..lastDayOfMonth - 6]
      possibleDays = map (\d -> fromGregorian year month d) days
      correctDays = filter (\d -> dayOfWeek d == intToDayOfWeek dayInt) possibleDays
  in  head correctDays

lastDayOfMonth' :: Integer -> Int -> Int
lastDayOfMonth' year month =
  case month of
    1  -> 31
    2  -> if isLeapYear year then 29 else 28
    3  -> 31
    4  -> 30
    5  -> 31
    6  -> 30
    7  -> 31
    8  -> 31
    9  -> 30
    10 -> 31
    11 -> 30
    12 -> 31
    _  -> error "Invalid month"

isLeapYear :: Integer -> Bool
isLeapYear year =
  (year `mod` 4 == 0) && ((year `mod` 100 /= 0) || (year `mod` 400 == 0))

intToDayOfWeek :: Int -> DayOfWeek
intToDayOfWeek 1 = Monday'
intToDayOfWeek 2 = Tuesday'
intToDayOfWeek 3 = Wednesday'
intToDayOfWeek 4 = Thursday'
intToDayOfWeek 5 = Friday'
intToDayOfWeek 6 = Saturday'
intToDayOfWeek 7 = Sunday'
intToDayOfWeek _ = error "Invalid day of week integer"

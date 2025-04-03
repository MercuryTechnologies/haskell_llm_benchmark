module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

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

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  case schedule of
    First  -> findDay year month weekday 1 7
    Second -> findDay year month weekday 8 14
    Third  -> findDay year month weekday 15 21
    Fourth -> findDay year month weekday 22 28
    Teenth -> findDay year month weekday 13 19
    Last   -> findLastDay year month weekday

findDay :: Integer -> Int -> Weekday -> Int -> Int -> Day
findDay y m w startDay endDay =
  let wVal       = weekdayToInt w
      candidates = [d | d <- [startDay..endDay], dayOfWeek y m d == wVal]
  in fromGregorian y m (head candidates)

dayOfWeek :: Integer -> Int -> Int -> Int
dayOfWeek y m d =
  let (_, _, w) = toWeekDate (fromGregorian y m d)
  in w

findLastDay :: Integer -> Int -> Weekday -> Day
findLastDay y m w =
  let wVal        = weekdayToInt w
      lastDay     = gregorianMonthLength y m
      candidates  = [d | d <- [lastDay, (lastDay - 1)..1], dayOfWeek y m d == wVal]
  in fromGregorian y m (head candidates)

weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

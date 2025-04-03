module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum, Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
    First  -> findNthWeekday 1
    Second -> findNthWeekday 2
    Third  -> findNthWeekday 3
    Fourth -> findNthWeekday 4
    Last   -> findLastWeekday
    Teenth -> findTeenthWeekday
  where
    firstDayOfMonth = fromGregorian year month 1
    (_, _, firstDayWeek) = toWeekDate firstDayOfMonth
    targetWeekday = fromEnum weekday + 1

    findNthWeekday n = addDays (fromIntegral daysToAdd) firstDayOfMonth
      where
        daysToAdd = (targetWeekday - firstDayWeek + 7) `mod` 7 + 7 * (n - 1)

    findLastWeekday = addDays (fromIntegral daysToAdd) lastDayOfMonth
      where
        lastDayOfMonth = fromGregorian year month (gregorianMonthLength year month)
        (_, _, lastDayWeek) = toWeekDate lastDayOfMonth
        daysToAdd = (targetWeekday - lastDayWeek + 7) `mod` 7

    findTeenthWeekday = addDays (fromIntegral daysToAdd) teenthStartDay
      where
        teenthStartDay = fromGregorian year month 13
        (_, _, teenthStartWeek) = toWeekDate teenthStartDay
        daysToAdd = (targetWeekday - teenthStartWeek + 7) `mod` 7

gregorianMonthLength :: Integer -> Int -> Int
gregorianMonthLength year month = if month == 2 then if isLeapYear year then 29 else 28 else if month `elem` [4, 6, 9, 11] then 30 else 31

isLeapYear :: Integer -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

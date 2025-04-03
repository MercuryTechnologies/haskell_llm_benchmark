module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    case schedule of
        First  -> findNthWeekday 1 weekday year month
        Second -> findNthWeekday 2 weekday year month
        Third  -> findNthWeekday 3 weekday year month
        Fourth -> findNthWeekday 4 weekday year month
        Last   -> findLastWeekday weekday year month
        Teenth -> findTeenthWeekday weekday year month

-- Convert our Weekday type to ISO weekday (1 = Monday, 7 = Sunday)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

-- Find the nth occurrence of a weekday in a month
findNthWeekday :: Int -> Weekday -> Integer -> Int -> Day
findNthWeekday n weekday year month = 
    let targetWeekdayNum = weekdayToInt weekday
        firstDay = fromGregorian year month 1
        (_, _, firstDayOfWeek) = toWeekDate firstDay
        -- Calculate days to add to reach the first occurrence of the target weekday
        daysToAdd = (targetWeekdayNum - firstDayOfWeek) `mod` 7
        -- Calculate the day of the first occurrence
        firstOccurrence = 1 + daysToAdd
        -- Calculate the day of the nth occurrence
        nthOccurrence = firstOccurrence + (n - 1) * 7
    in fromGregorian year month nthOccurrence

-- Find the last occurrence of a weekday in a month
findLastWeekday :: Weekday -> Integer -> Int -> Day
findLastWeekday weekday year month =
    let daysInMonth = gregorianMonthLength year month
        lastDay = fromGregorian year month daysInMonth
        (_, _, lastDayOfWeek) = toWeekDate lastDay
        targetWeekdayNum = weekdayToInt weekday
        -- Calculate days to subtract to reach the last occurrence of the target weekday
        daysToSubtract = (lastDayOfWeek - targetWeekdayNum) `mod` 7
        -- Calculate the day of the last occurrence
        lastOccurrence = daysInMonth - daysToSubtract
    in fromGregorian year month lastOccurrence

-- Find the 'teenth' occurrence of a weekday in a month (13th-19th)
findTeenthWeekday :: Weekday -> Integer -> Int -> Day
findTeenthWeekday weekday year month =
    let targetWeekdayNum = weekdayToInt weekday
        -- Check each day from 13 to 19
        teenthDays = [13..19]
        -- Find the day that matches our target weekday
        teenthDay = head [day | day <- teenthDays, 
                          let (_, _, dayOfWeek) = toWeekDate (fromGregorian year month day),
                          dayOfWeek == targetWeekdayNum]
    in fromGregorian year month teenthDay

module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
    First  -> nthWeekdayOfMonth 1 weekday year month
    Second -> nthWeekdayOfMonth 2 weekday year month
    Third  -> nthWeekdayOfMonth 3 weekday year month
    Fourth -> nthWeekdayOfMonth 4 weekday year month
    Last   -> lastWeekdayOfMonth weekday year month
    Teenth -> teenthWeekdayOfMonth weekday year month

-- Convert our Weekday to the day of week used in Data.Time (1=Monday, 7=Sunday)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday = 1
weekdayToInt Tuesday = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday = 4
weekdayToInt Friday = 5
weekdayToInt Saturday = 6
weekdayToInt Sunday = 7

-- Get the day of week (1-7) for a given date
dayOfWeek :: Day -> Int
dayOfWeek day = let (_, _, dow) = toWeekDate day in dow

-- Find the first occurrence of a weekday in a month
firstWeekdayOfMonth :: Weekday -> Integer -> Int -> Day
firstWeekdayOfMonth weekday year month = 
    let firstDay = fromGregorian year month 1
        targetDow = weekdayToInt weekday
        currentDow = dayOfWeek firstDay
        daysToAdd = (targetDow - currentDow + 7) `mod` 7
    in addDays (toInteger daysToAdd) firstDay

-- Find the nth occurrence of a weekday in a month
nthWeekdayOfMonth :: Int -> Weekday -> Integer -> Int -> Day
nthWeekdayOfMonth n weekday year month =
    let firstOccurrence = firstWeekdayOfMonth weekday year month
        daysToAdd = toInteger ((n - 1) * 7)
    in addDays daysToAdd firstOccurrence

-- Find the last occurrence of a weekday in a month
lastWeekdayOfMonth :: Weekday -> Integer -> Int -> Day
lastWeekdayOfMonth weekday year month =
    let lastDay = fromGregorian year month (gregorianMonthLength year month)
        targetDow = weekdayToInt weekday
        currentDow = dayOfWeek lastDay
        daysToSubtract = (currentDow - targetDow + 7) `mod` 7
    in addDays (negate $ toInteger daysToSubtract) lastDay

-- Find the occurrence of a weekday in the range 13-19 of a month
teenthWeekdayOfMonth :: Weekday -> Integer -> Int -> Day
teenthWeekdayOfMonth weekday year month =
    let day13 = fromGregorian year month 13
        targetDow = weekdayToInt weekday
        day13Dow = dayOfWeek day13
        daysToAdd = (targetDow - day13Dow + 7) `mod` 7
    in addDays (toInteger daysToAdd) day13

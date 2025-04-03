module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays, diffDays)
import Data.Time.Calendar.Month (isLeapYear)

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
    First   -> findNth weekday year month 1
    Second  -> findNth weekday year month 2
    Third   -> findNth weekday year month 3
    Fourth  -> findNth weekday year month 4
    Last    -> findLast weekday year month
    Teenth  -> findTeenth weekday year month

findNth :: Weekday -> Integer -> Int -> Int -> Day
findNth weekday year month n = head [fromGregorian year month day | day <- [1..daysInMonth year month], weekdayFor day == weekday, n == countWeekdays day]
  where
    countWeekdays d = length [x | x <- [1..d], weekdayFor x == weekday]
    weekdayFor d = toEnum ((fromGregorian year month d `mod` 7) + 1)
    
daysInMonth :: Integer -> Int -> Int
daysInMonth year month
    | month == 2 = if isLeapYear year then 29 else 28
    | month `elem` [4, 6, 9, 11] = 30
    | otherwise = 31

findLast :: Weekday -> Integer -> Int -> Day
findLast weekday year month = head [fromGregorian year month day | day <- reverse [1..daysInMonth year month], weekdayFor day == weekday]
  where
    weekdayFor d = toEnum ((fromGregorian year month d `mod` 7) + 1)

findTeenth :: Weekday -> Integer -> Int -> Day
findTeenth weekday year month = head [fromGregorian year month day | day <- [13..19], weekdayFor day == weekday]
  where
    weekdayFor d = toEnum ((fromGregorian year month d `mod` 7) + 1)

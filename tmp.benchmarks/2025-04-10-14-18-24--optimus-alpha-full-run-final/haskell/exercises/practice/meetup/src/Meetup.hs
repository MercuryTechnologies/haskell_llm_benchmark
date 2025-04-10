module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays)
import Data.Time.Calendar (gregorianMonthLength)
import Data.Time.Calendar (dayOfWeek)
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

-- Convert our Weekday to Data.Time.Calendar's dayOfWeek (Monday=1..Sunday=7)
weekdayToInt :: Weekday -> Int
weekdayToInt Monday    = 1
weekdayToInt Tuesday   = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday  = 4
weekdayToInt Friday    = 5
weekdayToInt Saturday  = 6
weekdayToInt Sunday    = 7

-- Get all days in the month that are the given weekday
daysInMonth :: Integer -> Int -> Weekday -> [Day]
daysInMonth year month weekday =
    filter isWeekday allDays
  where
    monthLength = gregorianMonthLength year month
    allDays = [fromGregorian year month d | d <- [1..monthLength]]
    isWeekday day = let (_,_,w) = toWeekDate day in w == weekdayToInt weekday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    case schedule of
        First  -> days !! 0
        Second -> days !! 1
        Third  -> days !! 2
        Fourth -> days !! 3
        Last   -> last days
        Teenth -> head $ filter isTeenth days
  where
    days = daysInMonth year month weekday
    isTeenth day =
        let (_,_,d) = toGregorian day
        in d >= 13 && d <= 19

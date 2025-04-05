module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)

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

weekdayToInt :: Weekday -> Int
weekdayToInt Monday = 1
weekdayToInt Tuesday = 2
weekdayToInt Wednesday = 3
weekdayToInt Thursday = 4
weekdayToInt Friday = 5
weekdayToInt Saturday = 6
weekdayToInt Sunday = 7

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    case schedule of
        Teenth -> findTeenth
        _ -> findNth
  where
    targetWday = weekdayToInt weekday
    firstDay = fromGregorian year month 1
    (_, _, firstWday) = toWeekDate firstDay
    dayOffset = (targetWday - firstWday) `mod` 7
    firstOccurrence = 1 + dayOffset
    
    findNth = case schedule of
        First  -> fromGregorian year month firstOccurrence
        Second -> fromGregorian year month (firstOccurrence + 7)
        Third  -> fromGregorian year month (firstOccurrence + 14)
        Fourth -> fromGregorian year month (firstOccurrence + 21)
        Last   -> let lastOccurrence = firstOccurrence + 7 * (weeksInMonth - 1)
                  in fromGregorian year month lastOccurrence
    
    findTeenth = head $ filter isTeenth $
        [fromGregorian year month d | d <- [13..19]] 
        where
            isTeenth day = 
                let (_, _, wday) = toWeekDate day
                in wday == targetWday
    
    weeksInMonth = let monthLen = gregorianMonthLength year month
                       remainingDays = monthLen - firstOccurrence
                   in 1 + remainingDays `div` 7

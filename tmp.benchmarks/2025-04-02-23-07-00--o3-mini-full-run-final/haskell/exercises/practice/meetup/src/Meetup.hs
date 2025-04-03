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
  let totalDays = gregorianMonthLength year month
      candidateDays = [ d | d <- [1..totalDays]
                          , let day = fromGregorian year month d
                          , let (_,_,dow) = toWeekDate day
                          , dow == weekdayToInt weekday
                          ]
  in case schedule of
       Teenth  -> let teenthDays = filter (\d -> d >= 13 && d <= 19) candidateDays
                  in fromGregorian year month (head teenthDays)
       First   -> fromGregorian year month (candidateDays !! 0)
       Second  -> fromGregorian year month (candidateDays !! 1)
       Third   -> fromGregorian year month (candidateDays !! 2)
       Fourth  -> fromGregorian year month (candidateDays !! 3)
       Last    -> fromGregorian year month (last candidateDays)

module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar ( Day, fromGregorian, gregorianMonthLength, dayOfWeek, DayOfWeek(..) , toGregorian)

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

-- Helper function to convert our Weekday to Data.Time.Calendar.DayOfWeek
weekdayToDayOfWeek :: Weekday -> DayOfWeek
weekdayToDayOfWeek Meetup.Monday    = Data.Time.Calendar.Monday
weekdayToDayOfWeek Meetup.Tuesday   = Data.Time.Calendar.Tuesday
weekdayToDayOfWeek Meetup.Wednesday = Data.Time.Calendar.Wednesday
weekdayToDayOfWeek Meetup.Thursday  = Data.Time.Calendar.Thursday
weekdayToDayOfWeek Meetup.Friday    = Data.Time.Calendar.Friday
weekdayToDayOfWeek Meetup.Saturday  = Data.Time.Calendar.Saturday
weekdayToDayOfWeek Meetup.Sunday    = Data.Time.Calendar.Sunday

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    let targetDayOfWeek = weekdayToDayOfWeek weekday

        -- Generate all days in the given month and year
        daysInMonth = [ fromGregorian year month d
                      | d <- [1 .. gregorianMonthLength year month] ]

        -- Filter days matching the target weekday
        matchingDays = filter (\d -> dayOfWeek d == targetDayOfWeek) daysInMonth

        -- Handle the 'Teenth' case separately
        findTeenthDay =
            let teenthCandidates = filter (\d -> let (_, _, dayOfMonth) = toGregorian d in dayOfMonth >= 13 && dayOfMonth <= 19) daysInMonth
            in head $ filter (\d -> dayOfWeek d == targetDayOfWeek) teenthCandidates -- Problem guarantees exactly one

    in case schedule of
         First  -> matchingDays !! 0
         Second -> matchingDays !! 1
         Third  -> matchingDays !! 2
         Fourth -> matchingDays !! 3
         Last   -> last matchingDays
         Teenth -> findTeenthDay

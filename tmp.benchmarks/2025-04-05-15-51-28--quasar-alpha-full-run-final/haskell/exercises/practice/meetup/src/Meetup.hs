module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Calendar (gregorianMonthLength)

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

-- Convert Day to Weekday
dayToWeekday :: Day -> Weekday
dayToWeekday day =
  case snd (sundayStartWeek day) of
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    6 -> Saturday
    _ -> error "Invalid weekday"

-- List all days in the month with their weekdays
daysInMonth :: Integer -> Int -> [(Day, Weekday)]
daysInMonth year month =
  let days = gregorianMonthLength year month
  in [ let d = fromGregorian year month day
       in (d, dayToWeekday d)
     | day <- [1..days]
     ]

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  let monthDays = daysInMonth year month
      filtered = case schedule of
        Teenth -> filter (\(d, wd) -> wd == weekday && isTeenth d) monthDays
        Last   -> reverse $ filter (\(_, wd) -> wd == weekday) monthDays
        First  -> filterByIndex 1
        Second -> filterByIndex 2
        Third  -> filterByIndex 3
        Fourth -> filterByIndex 4
  in case filtered of
       (d, _):_ -> d
       [] -> error "No such meetup day"
  where
    isTeenth d =
      let (_, _, dayOfMonth) = toGregorian d
      in dayOfMonth >= 13 && dayOfMonth <= 19

    filterByIndex n =
      let ws = filter (\(_, wd) -> wd == weekday) monthDays
      in drop (n - 1) ws

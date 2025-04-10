module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, addDays, DayOfWeek(..), toModifiedJulianDay)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Enum, Eq, Show)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = 
    let firstDay = fromGregorian year (fromIntegral month) 1
    in case schedule of
        First  -> findWeekday weekday firstDay
        Second -> addDays 7 (findWeekday weekday firstDay)
        Third  -> addDays 14 (findWeekday weekday firstDay)
        Fourth -> addDays 21 (findWeekday weekday firstDay)
        Last   -> findLastWeekday weekday (lastDayOfMonth year month)
        Teenth -> head [d | d <- [addDays 12 firstDay .. addDays 18 firstDay], dayOfWeek d == toEnum (fromEnum weekday)]

findWeekday :: Weekday -> Day -> Day
findWeekday targetWeekday startDate = 
    let startWeekday = dayOfWeek startDate
        diff = (fromEnum targetWeekday - fromEnum startWeekday) `mod` 7
    in addDays (fromIntegral diff) startDate

findLastWeekday :: Weekday -> Day -> Day
findLastWeekday targetWeekday endDate = 
    let endWeekday = dayOfWeek endDate
        diff = (fromEnum endWeekday - fromEnum targetWeekday) `mod` 7
    in addDays (fromIntegral (-diff)) endDate

lastDayOfMonth :: Integer -> Int -> Day
lastDayOfMonth year month = 
    let (y, m, _) = toGregorian (addDays 1 (fromGregorian year (fromIntegral month) 1))
    in addDays (-1) (fromGregorian y m 1)

dayOfWeek :: Day -> DayOfWeek
dayOfWeek = toEnum . fromIntegral . (`mod` 7) . toModifiedJulianDay

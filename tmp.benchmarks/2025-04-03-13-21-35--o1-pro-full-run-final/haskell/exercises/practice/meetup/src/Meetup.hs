module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar
  ( Day
  , fromGregorian
  , addDays
  , gregorianMonthLength
  )
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

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
    case schedule of
      First  -> addDays (0) first
      Second -> addDays 7  first
      Third  -> addDays 14 first
      Fourth -> addDays 21 first
      Teenth -> findMatchingDayFrom (fromGregorian year month 13) wdNum
      Last   -> findMatchingDayBackFrom lastOfMonth wdNum
  where
    -- Convert our Weekday to an Int where Monday=1..Sunday=7
    wdNum = toWeekdayNumber weekday

    -- The first day of the given month that matches 'weekday'
    first :: Day
    first = findMatchingDayFrom (fromGregorian year month 1) wdNum

    -- The last day of the given month
    lastOfMonth :: Day
    lastOfMonth = fromGregorian year month (gregorianMonthLength year month)

    ---------------------------------------------------------------------
    -- Helpers
    ---------------------------------------------------------------------
    toWeekdayNumber :: Weekday -> Int
    toWeekdayNumber Monday    = 1
    toWeekdayNumber Tuesday   = 2
    toWeekdayNumber Wednesday = 3
    toWeekdayNumber Thursday  = 4
    toWeekdayNumber Friday    = 5
    toWeekdayNumber Saturday  = 6
    toWeekdayNumber Sunday    = 7

    -- Return the (1..7) for the given Day (Mon=1..Sun=7)
    dayOfWeek :: Day -> Int
    dayOfWeek d = let (_,_,dw) = toWeekDate d in dw

    -- Given a start day and desired weekday, find the day >= start
    -- that matches the weekday
    findMatchingDayFrom :: Day -> Int -> Day
    findMatchingDayFrom base wanted =
      let change = (wanted - dayOfWeek base) `mod` 7
      in addDays (fromIntegral change) base

    -- Find the day <= start that matches the weekday
    findMatchingDayBackFrom :: Day -> Int -> Day
    findMatchingDayBackFrom base wanted =
      let dow     = dayOfWeek base
          change  = (dow - wanted) `mod` 7
      in addDays (fromIntegral (negate change)) base

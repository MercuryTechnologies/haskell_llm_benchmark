module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

-- Use Int to store total minutes from midnight (0 - 1439)
newtype Clock = Clock Int
  deriving Eq

minutesInDay :: Int
minutesInDay = 24 * 60

-- Normalize total minutes to be within the range [0, minutesInDay - 1]
normalizeMinutes :: Int -> Int
normalizeMinutes totalMinutes = (totalMinutes `mod` minutesInDay + minutesInDay) `mod` minutesInDay

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (normalizeMinutes (hour * 60 + min))

toString :: Clock -> String
toString (Clock totalMinutes) = printf "%02d:%02d" hour min
  where
    hour = totalMinutes `div` 60
    min = totalMinutes `mod` 60

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentMinutes) = Clock (normalizeMinutes (currentMinutes + deltaMinutes))
  where
    deltaMinutes = hour * 60 + min

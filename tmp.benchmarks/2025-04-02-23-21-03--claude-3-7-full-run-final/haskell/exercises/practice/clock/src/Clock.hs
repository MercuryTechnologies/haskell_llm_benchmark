module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock { hours :: Int, minutes :: Int }
  deriving Eq

-- | Normalize a clock to ensure hours are in 0-23 range and minutes in 0-59 range
normalize :: Int -> Int -> Clock
normalize h m = Clock hours' minutes'
  where
    totalMinutes = h * 60 + m
    adjustedMinutes = totalMinutes `mod` (24 * 60)
    hours' = adjustedMinutes `div` 60
    minutes' = adjustedMinutes `mod` 60

-- | Create a clock from hours and minutes
fromHourMin :: Int -> Int -> Clock
fromHourMin = normalize

-- | Convert a clock to a string in the format "HH:MM"
toString :: Clock -> String
toString (Clock h m) = pad h ++ ":" ++ pad m
  where
    pad n = if n < 10 then "0" ++ show n else show n

-- | Add a time delta (hours and minutes) to a clock
addDelta :: Int -> Int -> Clock -> Clock
addDelta deltaHours deltaMinutes (Clock h m) = 
  normalize (h + deltaHours) (m + deltaMinutes)

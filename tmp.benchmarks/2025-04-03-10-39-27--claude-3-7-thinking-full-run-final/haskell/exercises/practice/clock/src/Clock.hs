module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = 
  let totalMin = hour * 60 + min
      dayMinutes = 24 * 60
      -- Normalize to positive value within 24 hours
      normalizedMin = ((totalMin `mod` dayMinutes) + dayMinutes) `mod` dayMinutes
      h = normalizedMin `div` 60
      m = normalizedMin `mod` 60
  in Clock h m

toString :: Clock -> String
toString (Clock h m) = 
  let pad n = if n < 10 then "0" ++ show n else show n
  in pad h ++ ":" ++ pad m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = 
  fromHourMin (h + hour) (m + min)

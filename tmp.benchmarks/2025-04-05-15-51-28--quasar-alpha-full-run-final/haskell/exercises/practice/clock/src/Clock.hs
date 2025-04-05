module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = normalizeClock (Clock hour min)

toString :: Clock -> String
toString (Clock hour min) = padZero hour ++ ":" ++ padZero min
  where
    padZero n = let s = show n in if n < 10 then '0' : s else s

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock hour min) = normalizeClock (Clock (hour + dh) (min + dm))

normalizeClock :: Clock -> Clock
normalizeClock (Clock hour min) =
  let totalMinutes = hour * 60 + min
      totalMinutesMod = (totalMinutes `mod` (24 * 60) + (24 * 60)) `mod` (24 * 60)
      newHour = totalMinutesMod `div` 60
      newMin = totalMinutesMod `mod` 60
  in Clock newHour newMin

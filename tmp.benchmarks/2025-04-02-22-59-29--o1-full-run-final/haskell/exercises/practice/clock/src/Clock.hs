module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int
  deriving (Eq)

-- Normalize a minute value into the range 0..1439
modMinutes :: Int -> Int
modMinutes x = ((x `mod` 1440) + 1440) `mod` 1440

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
  Clock (modMinutes (hour * 60 + min))

toString :: Clock -> String
toString (Clock totalMins) =
  let h = totalMins `div` 60
      m = totalMins `mod` 60
      pad x = if x < 10 then "0" ++ show x else show x
  in pad h ++ ":" ++ pad m

addDelta :: Int -> Int -> Clock -> Clock
addDelta dh dm (Clock totalMins) =
  Clock (modMinutes (totalMins + dh * 60 + dm))

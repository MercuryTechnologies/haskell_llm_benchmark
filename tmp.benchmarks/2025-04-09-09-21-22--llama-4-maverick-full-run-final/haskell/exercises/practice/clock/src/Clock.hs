module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int deriving Eq

totalMinutes :: Int -> Int -> Int
totalMinutes hour min = (hour * 60 + min) `mod` 1440

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (totalMinutes hour min)

toString :: Clock -> String
toString (Clock totalMin) = 
  let hour = totalMin `div` 60
      min = totalMin `mod` 60
  in (if hour < 10 then "0" else "") ++ show hour ++ ":" ++ (if min < 10 then "0" else "") ++ show min

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock totalMin) = 
  let delta = totalMinutes hour min
  in Clock ((totalMin + delta) `mod` 1440)

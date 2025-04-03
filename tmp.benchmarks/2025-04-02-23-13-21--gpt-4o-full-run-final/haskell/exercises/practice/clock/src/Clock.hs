module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour * 60 + min) `mod` 1440)

toString :: Clock -> String
toString (Clock minutes) = 
  let (hour, min) = minutes `divMod` 60
  in padZero hour ++ ":" ++ padZero min
  where
    padZero n = if n < 10 then '0' : show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock minutes) = fromHourMin 0 (minutes + hour * 60 + min)

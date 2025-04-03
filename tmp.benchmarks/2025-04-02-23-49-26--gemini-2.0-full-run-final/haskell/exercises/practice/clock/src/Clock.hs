module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (normalizeHour totalMinutes) (normalizeMinute totalMinutes)
  where
    totalMinutes = hour * 60 + min

toString :: Clock -> String
toString (Clock hour min) = formatHour hour ++ ":" ++ formatMinute min
  where
    formatHour h = if h < 10 then "0" ++ show h else show h
    formatMinute m = if m < 10 then "0" ++ show m else show m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock currentHour currentMin) = fromHourMin (currentHour + hour) (currentMin + min)

normalizeMinute :: Int -> Int
normalizeMinute totalMinutes = mod totalMinutes 60

normalizeHour :: Int -> Int
normalizeHour totalMinutes = mod (div totalMinutes 60) 24

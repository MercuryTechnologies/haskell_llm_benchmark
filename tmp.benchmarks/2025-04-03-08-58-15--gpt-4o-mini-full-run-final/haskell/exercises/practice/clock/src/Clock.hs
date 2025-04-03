module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Int Int deriving Eq

instance Show Clock where
    show (Clock hour min) = let
        totalMinutes = (hour * 60 + min) `mod` 1440
        h = totalMinutes `div` 60
        m = totalMinutes `mod` 60
        in printf "%02d:%02d" h m

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (hour + (min `div` 60)) (min `mod` 60)

toString :: Clock -> String
toString clock = show clock

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = fromHourMin (h + hour) (m + min)

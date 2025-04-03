module Clock (addDelta, fromHourMin, toString) where

-- The Clock type represents a time in minutes past midnight.
newtype Clock = Clock Int
  deriving (Show)

instance Eq Clock where
    (Clock t1) == (Clock t2) = t1 == t2

-- Normalizes minutes to be between 0 and 1439 (inclusive).
normalize :: Int -> Int
normalize minutes = minutes `mod` 1440

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock $ normalize (hour * 60 + min)

toString :: Clock -> String
toString (Clock minutes) =
    let h = minutes `div` 60
        m = minutes `mod` 60
        pad n = if n < 10 then "0" ++ show n else show n
    in pad h ++ ":" ++ pad m

addDelta :: Int -> Int -> Clock -> Clock
addDelta addH addM (Clock current) =
    let totalMinutes = current + (addH * 60) + addM
    in Clock $ normalize totalMinutes

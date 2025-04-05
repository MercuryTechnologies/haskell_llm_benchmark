module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock Int  -- Internal representation is total minutes
  deriving Eq

-- Normalize hours and minutes to 24-hour format
normalize :: Int -> Int -> (Int, Int)
normalize hours mins =
  let total = hours * 60 + mins
      normalized = total `mod` (24 * 60)
  in (normalized `div` 60, normalized `mod` 60)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = 
  let (h, m) = normalize hour min
  in Clock (h * 60 + m)

toString :: Clock -> String
toString (Clock total) =
  let hours = total `div` 60
      mins = total `mod` 60
  in pad2 hours ++ ":" ++ pad2 mins
  where
    pad2 n = if n < 10 then '0' : show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock current) =
  let (h, m) = normalize hour min
      newTotal = current + h * 60 + m
  in Clock (newTotal `mod` (24 * 60))

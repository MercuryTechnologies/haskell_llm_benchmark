module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving (Show)

instance Eq Clock where
  (Clock h1 m1) == (Clock h2 m2) = (normalize h1 m1) == (normalize h2 m2)
    where
      normalize h m =
        let total = (h * 60 + m) `mod` (24 * 60)
            total' = if total < 0 then total + 24 * 60 else total
        in (total' `div` 60, total' `mod` 60)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = let (h, m) = normalize hour min in Clock h m
  where
    normalize h m =
      let total = (h * 60 + m) `mod` (24 * 60)
          total' = if total < 0 then total + 24 * 60 else total
      in (total' `div` 60, total' `mod` 60)

toString :: Clock -> String
toString (Clock h m) = pad h ++ ":" ++ pad m
  where
    pad n = if n < 10 then '0' : show n else show n

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = fromHourMin (h + hour) (m + min)

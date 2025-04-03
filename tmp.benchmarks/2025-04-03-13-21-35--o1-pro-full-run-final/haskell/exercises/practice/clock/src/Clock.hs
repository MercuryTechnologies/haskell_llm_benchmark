module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min =
    Clock (normalize (hour * 60 + min))

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock minutes) =
    Clock (normalize (minutes + hour * 60 + min))

toString :: Clock -> String
toString (Clock m) =
    let (h, mm) = m `divMod` 60
        hStr    = pad h
        mStr    = pad mm
    in hStr ++ ":" ++ mStr
  where
    pad x =
      let s = show x
      in if length s < 2 then '0' : s else s

normalize :: Int -> Int
normalize x = ((x `mod` 1440) + 1440) `mod` 1440

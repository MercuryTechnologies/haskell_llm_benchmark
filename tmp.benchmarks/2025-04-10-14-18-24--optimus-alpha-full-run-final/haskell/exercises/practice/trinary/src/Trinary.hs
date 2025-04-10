module Trinary (readTri, showTri) where

-- | Convert a trinary string to its decimal value.
--   Invalid input returns 0.
readTri :: String -> Integer
readTri s = go s 0
  where
    go [] acc = acc
    go (c:cs) acc =
      case c of
        '0' -> step 0
        '1' -> step 1
        '2' -> step 2
        _   -> 0  -- invalid character, return 0
      where
        step n = let acc' = acc * 3 + n in acc' `seq` go cs acc'

-- | Convert a decimal integer to its trinary string representation.
showTri :: Integer -> String
showTri n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = "0"
  | otherwise = reverse (toTri n)
  where
    toTri 0 = ""
    toTri x =
      let (q, r) = x `quotRem` 3
      in intToChar r : toTri q

    intToChar 0 = '0'
    intToChar 1 = '1'
    intToChar 2 = '2'
    intToChar _ = error "Unexpected remainder"

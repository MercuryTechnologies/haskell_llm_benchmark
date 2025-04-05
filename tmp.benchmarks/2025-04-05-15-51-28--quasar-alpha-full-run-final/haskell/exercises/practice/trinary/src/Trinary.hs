module Trinary (readTri, showTri) where

readTri :: String -> Int
readTri s = go s 0
  where
    go [] acc = acc
    go (c:cs) acc =
      case c of
        '0' -> step 0
        '1' -> step 1
        '2' -> step 2
        _   -> 0  -- invalid input yields 0
      where
        step d = let acc' = acc * 3 + d in acc' `seq` go cs acc'

showTri :: Int -> String
showTri n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = "0"
  | otherwise = reverse (toTri n)
  where
    toTri 0 = ""
    toTri x =
      let (q, r) = x `quotRem` 3
      in intToDigit r : toTri q

    intToDigit d
      | d == 0 = '0'
      | d == 1 = '1'
      | d == 2 = '2'
      | otherwise = error "Invalid digit in intToDigit"

module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri str = go (reverse str) 0 0
  where
    go [] _ acc = acc
    go (x:xs) idx acc
      | x `elem` "012" = go xs (idx + 1) (acc + (digitToInt x) * (3 ^ idx))
      | otherwise = 0

    digitToInt c = case c of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      _   -> 0

showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse (go n)
  where
    go 0 = ""
    go x = let (q, r) = x `divMod` 3
            in go q ++ show r

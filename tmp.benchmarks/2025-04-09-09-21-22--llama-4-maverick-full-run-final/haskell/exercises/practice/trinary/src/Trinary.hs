module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri s
  | all (`elem` "012") s = sum $ zipWith (*) (map digitToInt (reverse s)) (iterate (*3) 1)
  | otherwise            = 0
  where
    digitToInt :: Char -> Integer
    digitToInt c = read [c]

showTri :: Integer -> String
showTri 0 = "0"
showTri n = showTri' n ""
  where
    showTri' :: Integer -> String -> String
    showTri' 0 s = s
    showTri' n s = showTri' (n `div` 3) (show (n `mod` 3) ++ s)

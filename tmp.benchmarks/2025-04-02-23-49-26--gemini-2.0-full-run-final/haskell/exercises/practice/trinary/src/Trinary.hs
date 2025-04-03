module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri str = foldl (\acc c -> if c `elem` ['0', '1', '2'] then acc * 3 + toInteger (digitToInt c) else 0) 0 str
  where
    digitToInt :: Char -> Int
    digitToInt c = fromEnum c - fromEnum '0'

showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse $ showTri' n
  where
    showTri' 0 = ""
    showTri' m = case m `divMod` 3 of
      (0, r) -> intToDigit r : showTri' 0
      (q, r) -> intToDigit r : showTri' q

intToDigit :: Integer -> Char
intToDigit n = toEnum (fromInteger n + fromEnum '0')

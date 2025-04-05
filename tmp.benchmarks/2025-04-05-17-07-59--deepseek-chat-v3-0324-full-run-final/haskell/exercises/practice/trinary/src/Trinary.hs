module Trinary (readTri, showTri) where

readTri :: String -> Integer
readTri = foldl (\acc c -> if c `elem` ['0'..'2'] 
                           then 3 * acc + digitToInt c 
                           else 0) 0
  where digitToInt c = fromIntegral (fromEnum c - fromEnum '0')

showTri :: Integer -> String
showTri n
  | n <= 0 = "0"
  | otherwise = reverse $ go n
  where
    go 0 = ""
    go x = let (q, r) = x `divMod` 3
           in intToDigit r : go q
    intToDigit d = toEnum (fromIntegral d + fromEnum '0')

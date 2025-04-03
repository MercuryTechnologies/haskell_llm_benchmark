module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose [""] = []
transpose xs = map (getColumn xs) [0..maxLength-1]
  where
    maxLength = maximum $ map length xs
    
    getColumn :: [String] -> Int -> String
    getColumn strs idx = [getCharAt str idx | str <- strs, idx < length str || any (\s -> length s > idx) (drop (1 + findIndex str strs) strs)]
    
    getCharAt :: String -> Int -> Char
    getCharAt str idx
      | idx < length str = str !! idx
      | otherwise        = ' '
    
    findIndex :: String -> [String] -> Int
    findIndex _ [] = -1
    findIndex str (x:xs)
      | str == x  = 0
      | otherwise = 1 + findIndex str xs

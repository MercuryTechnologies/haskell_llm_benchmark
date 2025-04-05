module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose xs = 
    let maxLen = maximum (map length xs)
        padded = map (\s -> s ++ replicate (maxLen - length s) ' ') xs
    in if all null padded
       then []
       else map head padded : transpose (map tail padded)

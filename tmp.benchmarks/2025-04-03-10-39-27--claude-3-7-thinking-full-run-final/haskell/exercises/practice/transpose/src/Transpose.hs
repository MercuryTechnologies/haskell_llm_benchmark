module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose rows
    | all null rows = []
    | otherwise =
        let maxLength = maximum $ map length rows
        in [getCol j | j <- [0..maxLength-1]]
  where
    getCol j = 
        let col = [if j < length row then row !! j else ' ' | row <- rows]
            indices = [i | (i, row) <- zip [0..] rows, j < length row]
            lastRealCharIndex = if null indices then -1 else maximum indices
        in take (lastRealCharIndex + 1) col

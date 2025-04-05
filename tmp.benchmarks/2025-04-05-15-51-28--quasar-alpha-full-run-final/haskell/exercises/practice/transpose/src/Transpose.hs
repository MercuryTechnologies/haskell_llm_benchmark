module Transpose (transpose) where

transpose :: [String] -> [String]
transpose xs
  | null xs = []
  | otherwise = map trimRight $ transposeHelper padded
  where
    maxLen = maximum (map length xs)
    padded = map (padLeft maxLen) xs

    padLeft n s = replicate (n - length s) ' ' ++ s

    transposeHelper [] = []
    transposeHelper rows
      | all null rows = []
      | otherwise = map head rows : transposeHelper (map tail rows)

    trimRight s =
      let trimmed = reverse (dropWhile (== ' ') (reverse s))
      in if null trimmed then "" else trimmed

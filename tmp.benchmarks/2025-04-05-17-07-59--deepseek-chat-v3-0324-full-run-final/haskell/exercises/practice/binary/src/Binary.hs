module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal s
  | all (`elem` "01") s = foldl (\acc x -> 2 * acc + digitToInt x) 0 s
  | otherwise = 0
  where
    digitToInt '0' = 0
    digitToInt '1' = 1

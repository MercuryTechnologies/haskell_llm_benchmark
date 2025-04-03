module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
  | all (`elem` "01") xs = foldl (\acc digit -> acc * 2 + digitToInt digit) 0 xs
  | otherwise = 0
  where
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt _ = 0  -- This case should never be reached due to the validation

module Trinary (readTri, showTri) where

-- | Convert a trinary string to its decimal equivalent
-- Invalid trinary strings are treated as 0
readTri :: String -> Integer
readTri s
  | all isValidDigit s = foldl addDigit 0 s
  | otherwise = 0
  where
    isValidDigit c = c `elem` ['0', '1', '2']
    addDigit acc d = 3 * acc + digitToInt d
    digitToInt c = case c of
      '0' -> 0
      '1' -> 1
      '2' -> 2
      _   -> 0  -- This case shouldn't happen due to the validation

-- | Convert a decimal number to its trinary representation
showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse $ go n
  where
    go 0 = ""
    go x = let (q, r) = x `quotRem` 3
           in intToDigit r : go q
    intToDigit d = case d of
      0 -> '0'
      1 -> '1'
      2 -> '2'
      _ -> error "Unexpected remainder in trinary conversion"

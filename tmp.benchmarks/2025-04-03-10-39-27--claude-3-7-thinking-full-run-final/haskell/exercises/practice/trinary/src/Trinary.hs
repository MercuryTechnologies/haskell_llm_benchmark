module Trinary (readTri, showTri) where

-- | Convert a trinary string to a decimal number
readTri :: String -> Int
readTri s = if all isValidTrinaryDigit s
                then foldl (\acc c -> let val = acc * 3 + digitToInt c in val `seq` val) 0 s
                else 0
  where
    isValidTrinaryDigit c = c == '0' || c == '1' || c == '2'
    digitToInt c
      | c == '0' = 0
      | c == '1' = 1
      | c == '2' = 2
      | otherwise = error "Invalid trinary digit"  -- This should never happen due to the 'all' check

-- | Convert a decimal number to a trinary string
showTri :: Int -> String
showTri 0 = "0"
showTri n | n < 0 = "-" ++ showTri (-n)
          | otherwise = reverse $ go n
  where
    go 0 = ""
    go n = let (q, r) = n `divMod` 3
               q' = q `seq` q
               r' = r `seq` r
           in intToDigit r' : go q'
    intToDigit 0 = '0'
    intToDigit 1 = '1'
    intToDigit 2 = '2'
    intToDigit _ = error "This should never happen"

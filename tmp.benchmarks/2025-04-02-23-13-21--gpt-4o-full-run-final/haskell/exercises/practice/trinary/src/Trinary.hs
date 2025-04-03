module Trinary (readTri, showTri) where

import Prelude

-- Convert a trinary string to a decimal integer
readTri :: String -> Int
readTri = foldl convert 0
  where
    convert acc '0' = acc * 3
    convert acc '1' = acc * 3 + 1
    convert acc '2' = acc * 3 + 2
    convert _ _     = 0  -- Invalid character, return 0

-- Convert a decimal integer to a trinary string
showTri :: Int -> String
showTri 0 = "0"
showTri n = reverse (convert n)
  where
    convert 0 = ""
    convert x = let (q, r) = x `divMod` 3
                in toChar r : convert q
    toChar 0 = '0'
    toChar 1 = '1'
    toChar 2 = '2'

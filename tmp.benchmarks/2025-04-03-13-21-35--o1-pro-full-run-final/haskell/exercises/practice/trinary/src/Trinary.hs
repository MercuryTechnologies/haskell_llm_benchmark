module Trinary (readTri, showTri) where

-- Attempt to parse a string as a trinary (base-3) number and return its decimal value.
-- If the string contains an invalid trinary digit (anything other than 0, 1, or 2), return 0.
readTri :: String -> Integer
readTri xs =
  if all valid xs
    then myFoldl' step 0 xs
    else 0
  where
    valid c = c == '0' || c == '1' || c == '2'
    step acc c =
      let digit = case c of
                    '0' -> 0
                    '1' -> 1
                    '2' -> 2
                    _   -> 0
          acc' = acc * 3 + digit
      in acc' `seq` acc'

-- Convert a decimal number to its trinary (base-3) representation as a string.
-- For example, 302 -> "102012".
showTri :: Integer -> String
showTri 0 = "0"
showTri n = reverse (go n)
  where
    go 0 = ""
    go x =
      let (q, r) = x `divMod` 3
          c = case r of
                0 -> '0'
                1 -> '1'
                2 -> '2'
                _ -> '0'
      in c : go q

-- A strict left fold implementation using only Prelude.
myFoldl' :: (a -> b -> a) -> a -> [b] -> a
myFoldl' _ acc []     = acc
myFoldl' f acc (y:ys) =
  let acc' = f acc y
  in acc' `seq` myFoldl' f acc' ys

{-# LANGUAGE BangPatterns #-}
module Trinary (readTri, showTri) where

-- | Convert a trinary number, represented as a string, to its decimal equivalent.
--   If the string contains any invalid digit, the result is 0.
readTri :: (Integral a) => String -> a
readTri xs = go xs 0
  where
    go [] !acc = acc
    go (c:cs) !acc =
      case c of
        '0' -> go cs (acc * 3 + 0)
        '1' -> go cs (acc * 3 + 1)
        '2' -> go cs (acc * 3 + 2)
        _   -> 0  -- invalid digit yields 0

-- | Convert a non-negative decimal number to its trinary string representation.
--   For 0, the function returns "0".
showTri :: (Integral a) => a -> String
showTri n
  | n < 0     = error "Negative input not supported"
  | n == 0    = "0"
  | otherwise = reverse (go n [])
  where
    go 0 !acc = acc
    go !m !acc =
      let (q, r) = m `divMod` 3
          digit  = case r of
                     0 -> '0'
                     1 -> '1'
                     2 -> '2'
                     _ -> error "Unexpected remainder"
      in go q (digit : acc)

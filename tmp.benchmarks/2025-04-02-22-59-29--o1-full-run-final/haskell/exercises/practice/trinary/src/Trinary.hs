module Trinary (readTri, showTri) where

-- | Convert a trinary string (base 3) to its decimal (base 10) value.
--   Any invalid input is interpreted as zero.
--
--   Changed to be polymorphic so that it can return any Integral type
--   (including Int, which the tests expect).
readTri :: Integral a => String -> a
readTri s
  | any (\c -> c /= '0' && c /= '1' && c /= '2') s = 0
  | otherwise = go 0 s
  where
    go acc []     = acc
    go acc (c:cs) =
      let d = case c of
                 '0' -> 0
                 '1' -> 1
                 '2' -> 2
                 _   -> 0
          newAcc = acc * 3 + fromIntegral d
      in newAcc `seq` go newAcc cs

-- | Convert a decimal number (base 10) to its trinary (base 3) string representation.
--   Changed to be polymorphic over any Integral type (including Int),
--   which the tests require.
showTri :: (Integral a, Ord a) => a -> String
showTri 0 = "0"
showTri n
  | n < 0     = error "Negative numbers not supported."
  | otherwise = reverse (go n)
  where
    go 0 = []
    go x =
      let (q,r) = x `divMod` 3
          i     = fromIntegral r :: Int
      in digit i : go q

    digit 0 = '0'
    digit 1 = '1'
    digit 2 = '2'
    digit _ = error "Unexpected remainder."

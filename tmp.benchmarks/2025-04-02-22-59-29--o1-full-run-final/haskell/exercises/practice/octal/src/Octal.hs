module Octal (readOct, showOct) where

readOct :: (Integral a) => String -> a
readOct xs
  | any (\c -> c < '0' || c > '7') xs = 0
  | otherwise = foldl' step 0 xs
  where
    step acc c = acc * 8 + fromIntegral ((fromEnum c) - (fromEnum '0'))
    foldl' _ z [] = z
    foldl' f z (y:ys) =
      let z' = f z y
      in z' `seq` foldl' f z' ys

showOct :: (Integral a, Ord a) => a -> String
showOct n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = "0"
  | otherwise = reverse (go n "")
  where
    go 0 acc = acc
    go m acc =
      let (q, r) = m `divMod` 8
      in go q (toDigit r : acc)
    toDigit d = toEnum (fromEnum '0' + fromIntegral d)

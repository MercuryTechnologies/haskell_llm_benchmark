module Raindrops (convert) where

convert :: Int -> String
convert n
  | factors == "" = show n
  | otherwise     = factors
  where
    factors = pling ++ plang ++ plong
    pling = if n `mod` 3 == 0 then "Pling" else ""
    plang = if n `mod` 5 == 0 then "Plang" else ""
    plong = if n `mod` 7 == 0 then "Plong" else ""

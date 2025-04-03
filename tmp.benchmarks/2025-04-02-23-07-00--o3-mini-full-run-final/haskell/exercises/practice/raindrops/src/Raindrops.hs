module Raindrops (convert) where

convert :: Int -> String
convert n =
  let sound = concat [ if n `mod` 3 == 0 then "Pling" else ""
                     , if n `mod` 5 == 0 then "Plang" else ""
                     , if n `mod` 7 == 0 then "Plong" else ""
                     ]
  in if null sound then show n else sound

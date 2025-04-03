module Raindrops (convert) where

convert :: Int -> String
convert n =
    let s3 = if n `mod` 3 == 0 then "Pling" else ""
        s5 = if n `mod` 5 == 0 then "Plang" else ""
        s7 = if n `mod` 7 == 0 then "Plong" else ""
        drops = s3 ++ s5 ++ s7
    in if null(drops) then show n else drops

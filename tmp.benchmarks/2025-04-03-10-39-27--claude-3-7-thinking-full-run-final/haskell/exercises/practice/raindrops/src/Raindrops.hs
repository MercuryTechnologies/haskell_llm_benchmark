module Raindrops (convert) where

convert :: Int -> String
convert n = 
    if null sounds
        then show n
        else sounds
    where
        sounds = concat [
            if n `mod` 3 == 0 then "Pling" else "",
            if n `mod` 5 == 0 then "Plang" else "",
            if n `mod` 7 == 0 then "Plong" else "" ]

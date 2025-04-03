module Raindrops (convert) where

convert :: Int -> String
convert n
    | null sounds = show n
    | otherwise   = sounds
    where
        sounds = concat [sound | (factor, sound) <- factors, n `mod` factor == 0]
        factors = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode rails message = concat rows
  where
    rows = [getRow i | i <- [0..rails-1]]
    getRow r = [message !! j | j <- [0..length message - 1], (j `mod` (2 * rails - 2)) == r || (j `mod` (2 * rails - 2)) == (2 * rails - 2 - r)]

decode :: Int -> String -> String
decode rails message = concat [getRow i | i <- [0..rails-1]]
  where
    rowLengths = [length (getRow i) | i <- [0..rails-1]]
    getRow r = take (rowLengths !! r) (drop (sum (take r rowLengths)) message)

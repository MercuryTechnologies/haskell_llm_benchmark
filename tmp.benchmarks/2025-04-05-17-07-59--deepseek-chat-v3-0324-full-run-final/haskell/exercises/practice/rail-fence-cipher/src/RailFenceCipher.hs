module RailFenceCipher (encode, decode) where

import Data.List (sortOn, groupBy)
import Data.Char (isAlpha)
import Data.Function (on)

encode :: Int -> String -> String
encode rails msg = concatMap (map snd) $ sortAndGroup $ zip pattern filteredMsg
  where
    filteredMsg = filter isAlpha msg
    pattern = take (length filteredMsg) $ cycle $ concat [ [0..rails-1], reverse [1..rails-2] ]
    sortAndGroup = map (map snd) . groupBy ((==) `on` fst) . sortOn fst

decode :: Int -> String -> String
decode rails cipher = map snd $ sortOn fst $ zip indices cipher
  where
    indices = concat $ map snd $ sortOn fst $ zip pattern [0..]
    pattern = take (length cipher) $ cycle $ concat [ [0..rails-1], reverse [1..rails-2] ]

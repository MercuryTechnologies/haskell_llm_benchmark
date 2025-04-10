module Change (findFewestCoins) where

import Data.List (sort, minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | target < minimum coins = Nothing
  | otherwise = find target (sort coins)
  where
    find 0 _ = Just []
    find amt cs
      | amt < 0 = Nothing
      | null cs = Nothing
      | otherwise =
          let options = catMaybes [ fmap (c:) (find (amt - c) cs) | c <- reverse cs, c <= amt ]
          in if null options
             then Nothing
             else Just $ minimumBy (comparing length) options

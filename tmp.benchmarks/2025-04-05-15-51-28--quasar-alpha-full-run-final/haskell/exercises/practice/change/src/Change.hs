module Change (findFewestCoins) where

import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | target < minimum coins && target /= 0 = Nothing
  | otherwise =
      let sortedCoins = sortBy (flip compare) coins
          candidates = mapMaybe (tryCoin target sortedCoins) sortedCoins
      in if null candidates then Nothing else Just (minimumByLength candidates)

tryCoin :: Integer -> [Integer] -> Integer -> Maybe [Integer]
tryCoin target coins coin
  | coin > target = Nothing
  | otherwise =
      let remaining = target - coin
      in if remaining == 0
            then Just [coin]
            else case findFewestCoins remaining coins of
                    Just rest -> Just (coin : rest)
                    Nothing -> Nothing

minimumByLength :: [[a]] -> [a]
minimumByLength = foldr1 (\x y -> if length x <= length y then x else y)

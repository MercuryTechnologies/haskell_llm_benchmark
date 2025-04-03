module Change (findFewestCoins) where

import Data.List (sortBy)
import Data.Maybe (fromMaybe, listToMaybe)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | otherwise = solve target (sortBy (flip compare) coins)

solve :: Integer -> [Integer] -> Maybe [Integer]
solve 0 _ = Just []
solve target [] = Nothing
solve target coins = listToMaybe $ sortBy (\a b -> compare (length a) (length b)) solutions
  where
    solutions = [solution | coin <- coins, coin <= target,
                  Just subSolution <- [solve (target - coin) coins],
                  let solution = coin : subSolution]

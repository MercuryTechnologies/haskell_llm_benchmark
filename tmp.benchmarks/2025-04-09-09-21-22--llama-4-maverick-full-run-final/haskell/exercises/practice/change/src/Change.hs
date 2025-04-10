module Change (findFewestCoins) where

import Data.List (sort)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | otherwise = let coins' = reverse $ sort coins
                in helper target coins'
  where
    helper :: Integer -> [Integer] -> Maybe [Integer]
    helper 0 _ = Just []
    helper target [] = Nothing
    helper target coins@(coin:coins')
      | target >= coin = case helper (target - coin) coins of
                           Just coins -> Just (coin : coins)
                           Nothing -> helper target coins'
      | otherwise = helper target coins'

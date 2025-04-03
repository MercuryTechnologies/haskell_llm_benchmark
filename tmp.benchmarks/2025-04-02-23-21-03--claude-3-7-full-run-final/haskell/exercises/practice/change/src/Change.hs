module Change (findFewestCoins) where

import Data.List (sort)
import Data.Maybe (listToMaybe)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing                -- Negative change is not possible
  | target == 0 = Just []               -- No coins needed for zero change
  | null coins = Nothing                -- No coins available
  | minimum coins > target = Nothing    -- Smallest coin is larger than target
  | otherwise = findMinCoins target (reverse $ sort coins)  -- Sort in descending order

-- Dynamic programming approach to find minimum coins
findMinCoins :: Integer -> [Integer] -> Maybe [Integer]
findMinCoins target coins = 
  let 
    -- Create a table where dp[i] = minimum coins needed for amount i
    dp = buildTable target coins
  in 
    -- Reconstruct the solution from the dp table
    if dp !! fromIntegral target == Nothing
      then Nothing
      else reconstructSolution target coins dp

-- Build the dynamic programming table
buildTable :: Integer -> [Integer] -> [Maybe [Integer]]
buildTable target coins = 
  let 
    -- Initialize table with Nothing for all amounts except 0
    initial = Just [] : replicate (fromIntegral target) Nothing
  in 
    foldl (updateTable coins) initial [1..target]

-- Update the table for each amount
updateTable :: [Integer] -> [Maybe [Integer]] -> Integer -> [Maybe [Integer]]
updateTable coins dp amount = 
  let 
    -- For each coin, try to use it and see if it gives a better solution
    possibleSolutions = 
      [ fmap (coin :) (dp !! fromIntegral (amount - coin))
      | coin <- coins
      , coin <= amount
      , dp !! fromIntegral (amount - coin) /= Nothing
      ]
    
    -- Find the solution with minimum number of coins
    bestSolution = findBestSolution possibleSolutions
  in
    take (fromIntegral amount) dp ++ [bestSolution] ++ drop (fromIntegral amount + 1) dp

-- Find the solution with the minimum number of coins
findBestSolution :: [Maybe [Integer]] -> Maybe [Integer]
findBestSolution [] = Nothing
findBestSolution solutions = 
  listToMaybe $ 
    [ sol 
    | Just sol <- solutions
    , all (\(Just other) -> length sol <= length other) 
          [s | s@(Just _) <- solutions, s /= Just sol]
    ]

-- Reconstruct the solution from the dp table
reconstructSolution :: Integer -> [Integer] -> [Maybe [Integer]] -> Maybe [Integer]
reconstructSolution 0 _ _ = Just []
reconstructSolution amount coins dp = 
  case dp !! fromIntegral amount of
    Nothing -> Nothing
    Just [] -> Just []
    Just (coin:_) -> 
      fmap (coin :) (reconstructSolution (amount - coin) coins dp)

module Change (findFewestCoins) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

-- | Given a target amount and a list of coin denominations,
-- | find the smallest list of coins that sums up to the target.
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing  -- Cannot make change for a negative target
  | target == 0 = Just [] -- Base case: 0 target needs 0 coins
  | otherwise = Map.lookup target memo -- Look up the result for the target amount
  where
    -- Filter out non-positive coin denominations and sort them ascending.
    -- Sorting is not strictly necessary for correctness but can be slightly
    -- more efficient in some cases and makes reasoning easier.
    validCoins = sort [c | c <- coins, c > 0]

    -- Initialize the memoization table (Map) with the base case for amount 0.
    -- The map stores: amount -> Maybe [list of coins]
    initialMemo :: Map Integer (Maybe [Integer])
    initialMemo = Map.singleton 0 (Just [])

    -- Build the memoization table iteratively from amount 1 up to the target.
    memo :: Map Integer (Maybe [Integer])
    memo = foldl buildMemo initialMemo [1..target]

    -- | Function to compute and add the entry for a single amount to the memo table.
    buildMemo :: Map Integer (Maybe [Integer]) -> Integer -> Map Integer (Maybe [Integer])
    buildMemo currentMemo amount =
      let
        -- | Try forming 'amount' by adding each 'coin' to a solution for 'amount - coin'.
        tryCoin :: Integer -> Maybe [Integer]
        tryCoin coin
          -- Check if the coin is usable (not larger than the current amount)
          | coin > amount = Nothing
          | otherwise =
              -- Look up the best solution for the remaining amount (amount - coin)
              -- Map.lookup returns Maybe (Maybe [Integer]), hence the double check.
              case Map.lookup (amount - coin) currentMemo of
                Just (Just subResult) -> Just (coin : subResult) -- Prepend the coin used
                _                     -> Nothing -- No solution exists for 'amount - coin'

        -- Generate all possible valid solutions for the current 'amount'
        possibleSolutions :: [[Integer]]
        possibleSolutions = catMaybes $ map tryCoin validCoins

        -- Select the best solution (the one with the fewest coins)
        bestOption :: Maybe [Integer]
        bestOption = case possibleSolutions of
                       [] -> Nothing -- No solution found for this amount
                       -- Find the list with the minimum length
                       solutions -> Just $ minimumBy (comparing length) solutions

      -- Insert the best found solution (or Nothing) into the memo table for the current amount
      in Map.insert amount bestOption currentMemo

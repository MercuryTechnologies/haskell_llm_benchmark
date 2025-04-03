module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import Data.Maybe (isJust)
import Data.List (sort, minimumBy)
import Data.Ord (comparing)
import Control.Monad (join) -- Used for flattening Maybe (Maybe a) if needed, but not directly here.

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
    -- Target is negative: impossible.
    | target < 0 = Nothing
    -- Target is zero: requires no coins.
    | target == 0 = Just []
    -- Filter out non-positive coin denominations as they are problematic or useless.
    | let positiveCoins = filter (> 0) coins, null positiveCoins = Nothing -- No positive coins to make change.
    -- Target is positive but smaller than the smallest available positive coin: impossible.
    | target > 0 && target < minimum positiveCoins = Nothing
    -- Otherwise, proceed with dynamic programming.
    | otherwise = fmap sort (solutions ! target)
    where
        -- Use only positive coin values for the calculation.
        positiveCoins = filter (> 0) coins

        -- DP array: solutions ! i stores the optimal (shortest) list of coins for amount i.
        -- The array is defined recursively; lazy evaluation ensures values are computed as needed.
        solutions :: Array Integer (Maybe [Integer])
        solutions = listArray (0, target) (Just [] : map computeSolution [1..target])

        -- Compute the optimal solution for amount 'i' using previously computed solutions.
        computeSolution :: Integer -> Maybe [Integer]
        computeSolution i =
            let
                -- Generate potential solutions by trying each coin 'c' such that c <= i.
                -- A potential solution is formed by adding 'c' to the optimal solution for 'i - c'.
                candidateOptions :: [Maybe [Integer]]
                candidateOptions = map (\coin ->
                    -- Look up the solution for the remaining amount (i - coin).
                    -- solutions ! (i - coin) gives Maybe [Integer]
                    -- fmap (coin :) applies (coin :) inside the Maybe if it's Just.
                    fmap (coin :) (solutions ! (i - coin))
                    ) $ filter (<= i) positiveCoins -- Only consider coins <= current amount i.

                -- Filter out invalid paths (where 'i - c' was unreachable, resulting in Nothing).
                validSolutions :: [[Integer]]
                validSolutions = [sol | Just sol <- candidateOptions] -- List comprehension to extract lists from Just values.

            -- Select the shortest list among all valid solutions for amount 'i'.
            in case validSolutions of
                 [] -> Nothing -- Amount 'i' is unreachable with the given coins.
                 _  -> Just $ minimumBy (comparing length) validSolutions

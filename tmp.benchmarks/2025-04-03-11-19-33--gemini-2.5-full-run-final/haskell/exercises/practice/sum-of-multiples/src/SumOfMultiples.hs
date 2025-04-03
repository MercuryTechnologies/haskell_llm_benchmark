module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

-- | Given a list of factors and a limit, find the sum of all unique multiples
--   of the factors that are less than the limit.
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  let
    -- Helper function to get multiples of a single positive factor under the limit
    multiplesUnder :: Integer -> Integer -> [Integer]
    multiplesUnder factor lim
      | factor <= 0 = [] -- Ignore non-positive factors as they don't produce positive multiples in the required way.
      | otherwise   = takeWhile (< lim) [factor, factor * 2 ..]

    -- Generate multiples for each factor and concatenate the lists
    allMultiples = concatMap (`multiplesUnder` limit) factors

    -- Remove duplicates
    uniqueMultiples = nub allMultiples
  in
    -- Sum the unique multiples
    sum uniqueMultiples

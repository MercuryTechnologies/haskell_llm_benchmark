module Triplet (tripletsWithSum) where

import Data.Maybe (mapMaybe)

-- Function to generate Pythagorean triplets (a, b, c) such that a + b + c = sum.
-- Constraints: a < b < c and a^2 + b^2 = c^2.
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum =
  [ (a, b, c)
  | a <- [1 .. sum `div` 3]             -- Ensure a < b < c implies a < sum / 3
  , b <- [a + 1 .. (sum - a) `div` 2] -- Ensure b < c implies b < (sum - a) / 2
  , let c = sum - a - b               -- Calculate c based on the sum constraint
  , a * a + b * b == c * c            -- Check the Pythagorean condition
  ]

-- Note: The constraints a < b and b < c are implicitly handled by the ranges
-- and the calculation of c.
-- 1. The range for b starts from a + 1, ensuring a < b.
-- 2. The upper bound for b ensures b < (sum - a) / 2. Since c = sum - a - b,
--    this means b < c. If b = (sum - a) / 2, then c = sum - a - (sum - a) / 2 = (sum - a) / 2 = b.
--    So, the strict inequality b < (sum - a) / 2 ensures b < c. The integer division handles this correctly.

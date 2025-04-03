module Triplet (tripletsWithSum) where

-- | Finds all Pythagorean triplets (a, b, c) such that a < b < c
--   and a + b + c = sum.
tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [ (a, b, c) |
                        -- Determine the range for 'a'.
                        -- From a < b < c and a + b + c = sum, we deduce:
                        -- 3*a < a + b + c = sum => a < sum / 3.
                        -- Also, a + 2*b < sum. Since a < b (so a + 1 <= b),
                        -- a + 2*(a + 1) <= a + 2*b < sum => 3*a + 2 < sum => a <= floor((sum - 3) / 3)
                        -- A slightly tighter bound: 3a < sum - 2 => a <= (sum - 2) `div` 3
                        a <- [1 .. limitA],

                        -- Determine the range for 'b'.
                        -- We need a < b.
                        -- We also need b < c => b < sum - a - b => a + 2*b < sum => b < (sum - a) / 2.
                        -- So, b ranges from a + 1 up to floor((sum - a - 1) / 2).
                        b <- [a + 1 .. limitB a],

                        -- Calculate c based on the sum constraint.
                        let c = sum - a - b,

                        -- Check the Pythagorean condition.
                        -- The constraints a < b and b < c are implicitly handled by the loop ranges.
                        a*a + b*b == c*c
                      ]
  where
    -- Upper limit for 'a' based on 3a < sum - 2
    limitA :: Int
    limitA = (sum - 2) `div` 3

    -- Upper limit for 'b' based on a + 2b < sum
    limitB :: Int -> Int
    limitB a = (sum - a - 1) `div` 2

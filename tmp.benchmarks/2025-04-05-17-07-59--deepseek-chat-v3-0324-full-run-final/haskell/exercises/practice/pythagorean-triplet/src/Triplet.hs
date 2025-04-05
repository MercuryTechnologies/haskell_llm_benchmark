module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = 
    [ (a, b, c)
    | c <- [1..n `div` 2]  -- c can't be more than half the sum
    , b <- [1..c-1]        -- b must be less than c
    , let a = n - b - c    -- derive a from the sum
    , a > 0                -- a must be positive
    , a < b                -- a must be less than b
    , a^2 + b^2 == c^2     -- must satisfy Pythagorean theorem
    ]

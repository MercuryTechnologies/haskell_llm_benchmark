module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = 
  [(a, b, c) | 
    a <- [1..(n `div` 3)],
    let b_numerator = n * n - 2 * n * a,
    let b_denominator = 2 * (n - a),
    b_numerator `mod` b_denominator == 0,
    let b = b_numerator `div` b_denominator,
    let c = n - a - b,
    a < b, b < c,
    a^2 + b^2 == c^2]

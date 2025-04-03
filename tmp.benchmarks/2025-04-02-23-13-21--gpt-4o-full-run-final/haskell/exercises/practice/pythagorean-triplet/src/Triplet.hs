module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a, b, c) | a <- [1..sum], b <- [a+1..sum], let c = sum - a - b, a < b, b < c, a^2 + b^2 == c^2]

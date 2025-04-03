module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n <= 0    = Nothing  -- Invalid: squares are numbered from 1
  | n > 64    = Nothing  -- Invalid: chessboard only has 64 squares
  | otherwise = Just (2 ^ (n - 1))  -- 2^(n-1) grains on square n

total :: Integer
total = sum [2 ^ n | n <- [0..63]]  -- Sum of 2^0 through 2^63

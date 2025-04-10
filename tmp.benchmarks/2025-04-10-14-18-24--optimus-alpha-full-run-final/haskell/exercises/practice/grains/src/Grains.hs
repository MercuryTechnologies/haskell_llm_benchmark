module Grains (square, total) where

-- | Returns the number of grains on the nth square of the chessboard.
--   The first square is 1, the last is 64.
--   Returns Nothing if n is not in [1..64].
square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 = Nothing
  | otherwise       = Just (2 ^ (n - 1))

-- | Returns the total number of grains on the chessboard.
--   This is the sum of grains on all 64 squares.
total :: Integer
total = sum [2 ^ (n - 1) | n <- [1..64]]

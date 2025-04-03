module Triangle (rows) where

-- Function to calculate the next row of Pascal's triangle from the previous one.
-- It pads the previous row with 0s at the beginning and end, then sums adjacent elements.
-- Example: nextRow [1, 2, 1] -> zipWith (+) [0, 1, 2, 1] [1, 2, 1, 0] -> [1, 3, 3, 1]
nextRow :: [Integer] -> [Integer]
nextRow prev = zipWith (+) (0 : prev) (prev ++ [0])

-- Generates Pascal's triangle up to n rows.
rows :: Int -> [[Integer]]
rows n
  | n <= 0    = [] -- If n is zero or negative, return an empty list.
  | otherwise = take n $ iterate nextRow [1]
    -- iterate nextRow [1] generates an infinite list of rows starting with [1]:
    -- [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], ...]
    -- take n selects the first n rows from this infinite list.

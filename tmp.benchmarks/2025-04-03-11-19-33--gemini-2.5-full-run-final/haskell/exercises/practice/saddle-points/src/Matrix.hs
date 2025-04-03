module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, indices, (!))
import Data.Ix (range) -- Import range function for generating index ranges

-- | Find saddle points in a 2D Array (matrix).
-- A saddle point is an element that is >= all elements in its row
-- and <= all elements in its column.
-- Assumes the input is a 2D array with Int indices and Ord elements.
saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix
  -- Check if the array bounds are valid (min <= max for both dimensions).
  -- If not, the array is effectively empty or invalid, so return an empty list.
  | r1 > r2 || c1 > c2 = []
  | otherwise =
      -- Use a list comprehension to iterate through all indices (r, c) of the matrix.
      -- Keep only those indices that satisfy the isSaddle condition.
      [ (r, c) | (r, c) <- indices matrix, isSaddle (r, c) ]
  where
    -- Get the bounds of the array: ((minRow, minCol), (maxRow, maxCol))
    ((r1, c1), (r2, c2)) = bounds matrix
    -- Create a helper function for convenient access to matrix elements.
    val = (matrix !)

    -- | Check if the element at (r, c) is the maximum in its row.
    isRowMax :: Int -> Int -> Bool
    isRowMax r c =
      let v = val (r, c) -- The value of the element at (r, c)
          -- Generate all indices in the same row 'r'.
          rowIndices = [(r, j) | j <- range (c1, c2)]
      -- Check if the value 'v' is greater than or equal to all elements in its row.
      -- The 'all' function checks if the predicate holds true for every element in the list.
      in all (\idx -> val idx <= v) rowIndices

    -- | Check if the element at (r, c) is the minimum in its column.
    isColMin :: Int -> Int -> Bool
    isColMin r c =
      let v = val (r, c) -- The value of the element at (r, c)
          -- Generate all indices in the same column 'c'.
          colIndices = [(i, c) | i <- range (r1, r2)]
      -- Check if the value 'v' is less than or equal to all elements in its column.
      in all (\idx -> val idx >= v) colIndices

    -- | Check if the element at (r, c) is a saddle point
    -- (i.e., both a row maximum and a column minimum).
    isSaddle :: (Int, Int) -> Bool
    isSaddle (r, c) = isRowMax r c && isColMin r c

module Matrix (saddlePoints) where

import Data.Array (Array, bounds, range, (!))
import Data.Ix (Ix, rangeSize) -- Import Ix for type constraint and rangeSize for checking emptiness
import Data.List (maximum, minimum) -- Import list functions for finding max/min

-- | Find saddle points in a 2D array (matrix).
-- A saddle point is an element at index (r, c) such that it is
-- greater than or equal to every element in its row 'r' and
-- less than or equal to every element in its column 'c'.
--
-- The function takes an array with 2D indices (row, column) and
-- returns a list of coordinates (row, column) of the saddle points.
-- The indices 'r' (row) and 'c' (column) must be types that are instances of Ix.
-- The element type 'e' must be an instance of Ord for comparison.
saddlePoints :: (Ord e, Ix r, Ix c) => Array (r, c) e -> [(r, c)]
saddlePoints matrix
  -- If the array is empty (has zero elements), there are no saddle points.
  -- rangeSize calculates the total number of elements within the bounds.
  | rangeSize (bounds matrix) == 0 = []
  | otherwise =
      -- Iterate through all indices (r, c) within the array bounds using a list comprehension.
      [ (r, c) | (r, c) <- range arrayBounds, isSaddlePoint (r, c) ]
  where
    -- Get the bounds of the array: ((minRow, minCol), (maxRow, maxCol))
    arrayBounds@((minRow, minCol), (maxRow, maxCol)) = bounds matrix

    -- Helper function to check if the element at index (r, c) is a saddle point.
    isSaddlePoint (r, c) =
      let pointValue = matrix ! (r, c) -- The value at the current index (r, c).

          -- Generate all column indices for the current row 'r'.
          rowIndices = range (minCol, maxCol)
          -- Get all values in the current row 'r'.
          rowValues = [ matrix ! (r, j) | j <- rowIndices ]

          -- Generate all row indices for the current column 'c'.
          colIndices = range (minRow, maxRow)
          -- Get all values in the current column 'c'.
          colValues = [ matrix ! (i, c) | i <- colIndices ]

      -- Check the saddle point conditions:
      -- 1. The value must be greater than or equal to the maximum value in its row.
      -- 2. The value must be less than or equal to the minimum value in its column.
      -- Note: maximum and minimum are safe to use here because the initial check
      --       ensures rangeSize > 0, implying rowValues and colValues will be non-empty.
      in pointValue >= maximum rowValues && pointValue <= minimum colValues

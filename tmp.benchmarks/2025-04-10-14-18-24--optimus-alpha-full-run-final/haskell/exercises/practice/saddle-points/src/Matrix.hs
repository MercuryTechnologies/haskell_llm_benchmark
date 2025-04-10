module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), Ix, range)

-- | Find all saddle points in the matrix.
--   A saddle point is an index (i, j) such that:
--   - It is the maximum in its row.
--   - It is the minimum in its column.
saddlePoints :: (Ix i, Ix j, Ord e) => Array (i, j) e -> [(i, j)]
saddlePoints matrix = 
    [ (i, j)
    | i <- rowIndices
    , j <- colIndices
    , let val = matrix ! (i, j)
    , val == rowMax i
    , val == colMin j
    ]
  where
    ((rowLo, colLo), (rowHi, colHi)) = bounds matrix
    rowIndices = range (rowLo, rowHi)
    colIndices = range (colLo, colHi)

    -- For a given row, find the maximum value in that row
    rowMax i = maximum [matrix ! (i, j) | j <- colIndices]
    -- For a given column, find the minimum value in that column
    colMin j = minimum [matrix ! (i, j) | i <- rowIndices]

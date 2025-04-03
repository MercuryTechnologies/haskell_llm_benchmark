module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!))
import Data.Ix (Ix, range)

-- | Return a list of (row, column) positions in the array
--   that are the largest in their row, but smallest in their column.
saddlePoints :: (Ix i, Ix j, Ord e) => Array (i, j) e -> [(i, j)]
saddlePoints matrix =
    let ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    in [ (r, c)
       | (r, c) <- indices matrix
       , let val       = matrix ! (r, c)
             rowValues = [ matrix ! (r, x) | x <- range (minCol, maxCol) ]
             colValues = [ matrix ! (y, c) | y <- range (minRow, maxRow) ]
       , val == maximum rowValues
       , val == minimum colValues
       ]

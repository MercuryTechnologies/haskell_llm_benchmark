module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), indices)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = filter isSaddlePoint (indices matrix)
  where
    (minBound, maxBound) = bounds matrix
    (minRow, minCol) = minBound
    (maxRow, maxCol) = maxBound
    
    isSaddlePoint (r, c) = 
      let value = matrix ! (r, c)
          rowValues = [matrix ! (r, col) | col <- [minCol..maxCol]]
          colValues = [matrix ! (row, c) | row <- [minRow..maxRow]]
          isRowMax = value == maximum rowValues
          isColMin = value == minimum colValues
      in isRowMax && isColMin

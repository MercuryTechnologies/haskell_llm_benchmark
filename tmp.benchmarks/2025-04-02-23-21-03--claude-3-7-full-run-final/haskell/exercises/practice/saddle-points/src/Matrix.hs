module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!), Ix)
import Data.List (groupBy)
import Data.Function (on)

saddlePoints :: (Ix i, Ix j, Ord e) => Array (i, j) e -> [(i, j)]
saddlePoints matrix
  | null (indices matrix) = []
  | otherwise = filter isSaddlePoint (indices matrix)
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
    
    -- Group indices by row
    rows = groupBy ((==) `on` fst) (indices matrix)
    -- Group indices by column
    cols = groupBy ((==) `on` snd) (indices matrix)
    
    -- Find maximum value in each row
    rowMaxs = [(r, maximum [matrix ! (r, c) | c <- [minCol..maxCol]]) | r <- [minRow..maxRow]]
    -- Find minimum value in each column
    colMins = [(c, minimum [matrix ! (r, c) | r <- [minRow..maxRow]]) | c <- [minCol..maxCol]]
    
    -- Check if a point is a saddle point
    isSaddlePoint idx@(r, c) = 
      let val = matrix ! idx
          rowMax = lookup r rowMaxs
          colMin = lookup c colMins
      in Just val == rowMax && Just val == colMin

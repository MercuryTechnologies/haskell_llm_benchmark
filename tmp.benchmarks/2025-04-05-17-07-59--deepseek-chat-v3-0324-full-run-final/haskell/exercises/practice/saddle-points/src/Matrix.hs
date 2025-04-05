module Matrix (saddlePoints) where

import Data.Array (Array, bounds, assocs, listArray, elems, (!))
import Data.List (transpose)

saddlePoints :: (Ord e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix =
  let ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
      rows = [ [matrix ! (r, c) | c <- [minCol..maxCol]] | r <- [minRow..maxRow]]
      cols = transpose rows
      rowMaxima = map maximum rows
      colMinima = map minimum cols
  in [ (r+minRow, c+minCol) 
     | (r, row) <- zip [0..] rows
     , (c, val) <- zip [0..] row
     , val == rowMaxima !! r
     , val == colMinima !! c
     ]

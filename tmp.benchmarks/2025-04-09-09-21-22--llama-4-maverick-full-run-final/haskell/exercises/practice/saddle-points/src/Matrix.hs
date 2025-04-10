module Matrix (saddlePoints) where

import Data.Array (Array, bounds, Ix, (!))

saddlePoints :: (Ord e, Ix i) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = 
    let ((minRow, minCol), (maxRow, maxCol)) = bounds matrix
        rows = [[(row, col) | col <- [minCol..maxCol]] | row <- [minRow..maxRow]]
        cols = [[(row, col) | row <- [minRow..maxRow]] | col <- [minCol..maxCol]]
        isMaxInRow (row, col) = matrix ! (row, col) == maximum [matrix ! (row, c) | c <- [minCol..maxCol]]
        isMinInCol (row, col) = matrix ! (row, col) == minimum [matrix ! (r, col) | r <- [minRow..maxRow]]
    in [(row, col) | row <- [minRow..maxRow], col <- [minCol..maxCol], isMaxInRow (row, col), isMinInCol (row, col)]

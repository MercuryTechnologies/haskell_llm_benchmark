module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!), assocs)
import Data.Ix (Ix)
import Data.List (groupBy, maximumBy, minimumBy)
import Data.Ord (comparing)

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = [(r, c) | ((r, c), v) <- assocs matrix, isSaddlePoint r c v]
  where
    ((minRow, minCol), (maxRow, maxCol)) = bounds matrix

    isSaddlePoint r c v = v == maxInRow r && v == minInCol c

    maxInRow r = maximum [matrix ! (r, c) | c <- [minCol..maxCol]]
    minInCol c = minimum [matrix ! (r, c) | r <- [minRow..maxRow]]

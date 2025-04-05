module Matrix (saddlePoints) where

import Data.Array (Array, Ix, bounds, (!), indices)
import Data.List (groupBy, maximumBy, minimumBy)
import Data.Function (on)

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix = 
    [ pos
    | pos@(r, c) <- indices matrix
    , let rowElems = [matrix ! (r, c') | c' <- cols]
          colElems = [matrix ! (r', c) | r' <- rows]
          val = matrix ! pos
    , val == maximum rowElems
    , val == minimum colElems
    ]
  where
    ((rowMin, colMin), (rowMax, colMax)) = bounds matrix
    rows = [rowMin .. rowMax]
    cols = [colMin .. colMax]

module Matrix (saddlePoints) where

import Data.Ix (Ix)
import Data.Array (Array, bounds, (!))

saddlePoints :: (Ix i, Num e, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints matrix =
  let ((rStart, cStart), (rEnd, cEnd)) = bounds matrix
  in [ (r, c) 
     | r <- enumFromTo rStart rEnd
     , c <- enumFromTo cStart cEnd
     , let val = matrix!(r,c)
     , val == maximum [matrix!(r, col) | col <- enumFromTo cStart cEnd]
     , val == minimum [matrix!(row, c) | row <- enumFromTo rStart rEnd]
     ]

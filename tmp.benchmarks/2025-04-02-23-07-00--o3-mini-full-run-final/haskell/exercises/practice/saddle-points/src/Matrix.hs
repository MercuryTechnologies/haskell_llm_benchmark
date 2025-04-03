module Matrix (saddlePoints) where

import Data.Array (Array, bounds, (!))
import Data.Ix (range)

-- | Given a grid (as a two-dimensional Array) where the rows represent the east-west
--   direction and the columns represent the north-south direction, a "saddle point"
--   (or potential tree house spot) is one that is the largest in its row and smallest
--   in its column.
saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =
  let ((rmin, cmin), (rmax, cmax)) = bounds matrix
      rows = [rmin .. rmax]
      cols = [cmin .. cmax]
      candidates = [(r, c) | r <- rows, c <- cols]
  in [ (r, c)
     | (r, c) <- candidates
     , let val = matrix ! (r, c)
     , val == maximum [matrix ! (r, c') | c' <- cols]
     , val == minimum [matrix ! (r', c) | r' <- rows]
     ]

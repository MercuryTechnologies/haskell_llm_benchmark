module Matrix (saddlePoints) where

import Data.Array (Array, bounds, elems, (!))

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = [(r, c) | (r, c) <- allIndices, isGoodTree r c]
  where
    allIndices = [(r, c) | r <- [rMin..rMax], c <- [cMin..cMax]]
    ((rMin, cMin), (rMax, cMax)) = bounds matrix
    isGoodTree r c =
      let height = matrix ! (r, c)
          row = [matrix ! (r, c') | c' <- [cMin..cMax]]
          col = [matrix ! (r', c) | r' <- [rMin..rMax]]
      in height == maximum row && height == minimum col

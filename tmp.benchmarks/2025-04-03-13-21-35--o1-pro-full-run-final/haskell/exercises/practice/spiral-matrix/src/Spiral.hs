module Spiral (spiral) where

import Data.List (sortOn, groupBy)

-- | Returns a square matrix of size n, filled with natural numbers
--   in a clockwise spiral order.
spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise =
      let coords = spiralCoords n
          labeled = zip coords [1..n*n]
          sorted  = sortOn (\((r, c), _) -> (r, c)) labeled
          grouped = groupBy (\((r1, _), _) ((r2, _), _) -> r1 == r2) sorted
      in map (map snd) grouped

-- | Generate the list of (row, col) coordinates in spiral order.
spiralCoords :: Int -> [(Int, Int)]
spiralCoords n =
  concat [ ringCoords n layer
         | layer <- [0 .. (n - 1) `div` 2]
         ]

-- | Generate the coordinates of one ring/layer in the spiral.
ringCoords :: Int -> Int -> [(Int, Int)]
ringCoords size k =
  topRow k ++ rightCol k ++ bottomRow k ++ leftCol k
  where
    topRow k      = [ (k, c) | c <- [k .. size - k - 1] ]
    rightCol k    = [ (r, size - k - 1) | r <- [k + 1 .. size - k - 2] ]
    bottomRow k
      | size - k - 1 > k
      = [ (size - k - 1, c) | c <- [size - k - 1, size - k - 2 .. k] ]
      | otherwise
      = []
    leftCol k
      | k < size - k - 1
      = [ (r, k) | r <- [size - k - 2, size - k - 3 .. k + 1] ]
      | otherwise
      = []

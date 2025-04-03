module Spiral (spiral) where

import Data.List (sortBy)

spiral :: Int -> [[Int]]
spiral n 
  | n <= 0    = []
  | otherwise = chunksOf n $ map snd sortedValues
  where
    coords = spiralCoords n
    values = zip coords [1..]
    sortedValues = sortBy compareCoords values
    compareCoords ((r1, c1), _) ((r2, c2), _) = 
      case compare r1 r2 of
        EQ -> compare c1 c2
        other -> other

spiralCoords :: Int -> [(Int, Int)]
spiralCoords n = go 0 0 (n-1) (n-1)
  where
    go r1 c1 r2 c2
      | r1 > r2 || c1 > c2 = []
      | r1 == r2 = [(r1, c) | c <- [c1..c2]]
      | c1 == c2 = [(r, c1) | r <- [r1..r2]]
      | otherwise = 
          [(r1, c) | c <- [c1..c2]] ++                  -- top
          [(r, c2) | r <- [r1+1..r2]] ++                -- right
          [(r2, c) | c <- reverse [c1..c2-1]] ++        -- bottom
          [(r, c1) | r <- reverse [r1+1..r2-1]] ++      -- left
          go (r1+1) (c1+1) (r2-1) (c2-1)                -- inner spiral

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

module Spiral (spiral) where

import Data.Array (Array, array, (!))

spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise = toLists $ spiralArray n

spiralArray :: Int -> Array (Int, Int) Int
spiralArray n = array ((1, 1), (n, n)) $ zip indexList valueList
  where
    indexList = spiralIndices n
    valueList = [1..n*n]

spiralIndices :: Int -> [(Int, Int)]
spiralIndices n = spiralIndices' n 1 1 1

spiralIndices' :: Int -> Int -> Int -> Int -> [(Int, Int)]
spiralIndices' 0 _ _ _ = []
spiralIndices' 1 x y _ = [(x, y)]
spiralIndices' n x y dir =
  let
    (newX, newY, newDir) = nextCoord n x y dir
  in
    (x, y) : spiralIndices' (n - 1) newX newY newDir

nextCoord :: Int -> Int -> Int -> Int -> (Int, Int, Int)
nextCoord n x y dir
  | dir == 1 && (y == n || used (x, y + 1)) = (x + 1, y, 2)
  | dir == 2 && (x == n || used (x + 1, y)) = (x, y - 1, 3)
  | dir == 3 && (y == 1 || used (x, y - 1)) = (x - 1, y, 4)
  | dir == 4 && (x == 1 || used (x - 1, y)) = (x, y + 1, 1)
  | dir == 1 = (x, y + 1, dir)
  | dir == 2 = (x + 1, y, dir)
  | dir == 3 = (x, y - 1, dir)
  | dir == 4 = (x - 1, y, dir)
  | otherwise = error "Invalid direction"
  where
    used coord = coord `elem` usedCoords
    usedCoords = drop 1 $ spiralIndices (n + 1)

toList :: Array (Int, Int) Int -> [Int]
toList arr = [arr ! (i, j) | i <- [1..n], j <- [1..n]]
  where
    (_, (n, _)) = bounds arr

toLists :: Array (Int, Int) Int -> [[Int]]
toLists arr = [[arr ! (i, j) | j <- [1..n]] | i <- [1..n]]
  where
    (_, (n, _)) = bounds arr

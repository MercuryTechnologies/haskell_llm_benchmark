module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n = spiralFill n 1 0 0 (replicate n (replicate n 0)) 0

spiralFill :: Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Int]]
spiralFill n num x y matrix dir
  | num > n * n = matrix
  | otherwise = spiralFill n (num + 1) newX newY newMatrix newDir
  where
    (newX, newY, newMatrix, newDir) = move n num x y matrix dir

move :: Int -> Int -> Int -> Int -> [[Int]] -> Int -> (Int, Int, [[Int]], Int)
move n num x y matrix dir
  | dir == 0 && (y == n || matrix !! x !! y /= 0) = move n num (x + 1) (y - 1) matrix 1
  | dir == 1 && (x == n || matrix !! x !! y /= 0) = move n num (x - 1) (y - 1) matrix 2
  | dir == 2 && (y < 0 || matrix !! x !! y /= 0) = move n num (x - 1) (y + 1) matrix 3
  | dir == 3 && (x < 0 || matrix !! x !! y /= 0) = move n num (x + 1) (y + 1) matrix 0
  | otherwise = (newX, newY, updateMatrix x y num matrix, dir)
  where
    (newX, newY) = case dir of
      0 -> (x, y + 1)
      1 -> (x + 1, y)
      2 -> (x, y - 1)
      3 -> (x - 1, y)

updateMatrix :: Int -> Int -> Int -> [[Int]] -> [[Int]]
updateMatrix x y num matrix = take x matrix ++ [take y (matrix !! x) ++ [num] ++ drop (y + 1) (matrix !! x)] ++ drop (x + 1) matrix

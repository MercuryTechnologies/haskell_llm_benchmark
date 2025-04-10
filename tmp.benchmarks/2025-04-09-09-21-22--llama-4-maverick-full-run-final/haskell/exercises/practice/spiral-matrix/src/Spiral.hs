module Spiral (spiral) where

import Data.List (transpose)

spiral :: Int -> [[Int]]
spiral size = spiralHelper size 1 []

spiralHelper :: Int -> Int -> [[Int]] -> [[Int]]
spiralHelper 0 _ matrix = matrix
spiralHelper size num matrix = 
    let newRow = [num..num + size - 1]
        newMatrix = if null matrix then [newRow] else 
            let rotatedMatrix = map reverse (transpose matrix)
            in newRow : rotatedMatrix
        newNum = num + size
        newSize = size - 1
    in spiralHelper newSize newNum newMatrix

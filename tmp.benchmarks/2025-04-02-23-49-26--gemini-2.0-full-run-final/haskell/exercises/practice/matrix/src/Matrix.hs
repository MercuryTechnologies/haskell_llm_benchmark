module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (transpose, lines, words)

data Matrix a = Matrix {
    matrixData :: [[a]],
    numRows :: Int,
    numCols :: Int
} deriving (Eq, Show)

cols :: Matrix a -> Int
cols matrix = numCols matrix

column :: Int -> Matrix a -> Vector a
column x matrix = 
    let col = (matrixData matrix) in
    V.fromList (map (!! x) col)

flatten :: Matrix a -> Vector a
flatten matrix = V.fromList $ concat (matrixData matrix)

fromList :: [[a]] -> Matrix a
fromList xss = 
    let numRows = length xss
        numCols = length (head xss)
    in Matrix xss numRows numCols

fromString :: Read a => String -> Matrix a
fromString xs = 
    let rows = map (map read . words) (lines xs)
    in fromList rows

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) matrix =
    let flat = flatten matrix
        list = V.toList flat
        reshapedList = chunksOf newCols list
    in fromList reshapedList
  where
    chunksOf _ [] = []
    chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest

row :: Int -> Matrix a -> Vector a
row x matrix = V.fromList (matrixData matrix !! x)

rows :: Matrix a -> Int
rows matrix = numRows matrix

shape :: Matrix a -> (Int, Int)
shape matrix = (numRows matrix, numCols matrix)

transpose :: Matrix a -> Matrix a
transpose matrix = fromList (Data.List.transpose (matrixData matrix))

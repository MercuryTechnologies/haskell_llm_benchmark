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

-- Matrix is represented as a vector of rows
data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

-- Number of columns in the matrix
cols :: Matrix a -> Int
cols (Matrix m) = if V.null m then 0 else V.length (V.head m)

-- Get a specific column
column :: Int -> Matrix a -> Vector a
column _ (Matrix m) | V.null m = V.empty
column n (Matrix m)
    | n >= 0 && n < cols' = V.map (\r -> r V.! n) m
    | otherwise = error $ "Column index " ++ show n ++ " out of bounds (0," ++ show (cols' - 1) ++ ")"
  where
    cols' = if V.null m then 0 else V.length (V.head m)

-- Flatten matrix to a single vector
flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat (V.toList m)

-- Create a matrix from a list of lists
fromList :: [[a]] -> Matrix a
fromList [] = Matrix V.empty
fromList xss = Matrix (V.fromList (map V.fromList xss))

-- Create a matrix from a string
fromString :: Read a => String -> Matrix a
fromString xs = fromList [[read s | s <- words line] | line <- lines xs]

-- Reshape matrix to new dimensions
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m = 
    let flat = flatten m
        total = r * c
    in if V.length flat /= total
       then error "Cannot reshape: incompatible dimensions"
       else Matrix (V.generate r (\i -> V.slice (i * c) c flat))

-- Get a specific row
row :: Int -> Matrix a -> Vector a
row n (Matrix m)
    | n >= 0 && n < V.length m = m V.! n
    | otherwise = error $ "Row index " ++ show n ++ " out of bounds (0," ++ show (V.length m - 1) ++ ")"

-- Number of rows in the matrix
rows :: Matrix a -> Int
rows (Matrix m) = V.length m

-- Get the shape of the matrix (rows, columns)
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- Transpose the matrix
transpose :: Matrix a -> Matrix a
transpose m@(Matrix vec)
    | V.null vec = Matrix V.empty
    | otherwise = 
        let (r, c) = shape m
        in Matrix (V.generate c (\i -> column i m))

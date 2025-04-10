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
import qualified Data.List as L
import Data.Char (isSpace)

-- Matrix is a wrapper around a Vector of Vectors (row-major)
newtype Matrix a = Matrix (Vector (Vector a))
    deriving (Eq, Show)

-- Number of columns in the matrix
cols :: Matrix a -> Int
cols (Matrix v)
    | V.null v  = 0
    | otherwise = V.length (V.head v)

-- Get the specified column (0-based)
column :: Int -> Matrix a -> Vector a
column n (Matrix v) = V.map (V.! n) v

-- Flatten the matrix into a single vector (row-major order)
flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

-- Construct a matrix from a list of lists
fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ V.fromList (map V.fromList xss)

-- Parse a string into a matrix
fromString :: Read a => String -> Matrix a
fromString s = fromList $ map (map read . words) (lines s)

-- Reshape the matrix to new dimensions (row-major order)
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m =
    let flat = V.toList (flatten m)
        rowsList = take r $ map (V.fromList . take c) $ iterate (drop c) flat
    in Matrix $ V.fromList rowsList

-- Get the specified row (0-based)
row :: Int -> Matrix a -> Vector a
row n (Matrix v) = v V.! n

-- Number of rows in the matrix
rows :: Matrix a -> Int
rows (Matrix v) = V.length v

-- Shape of the matrix (rows, cols)
shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

-- Transpose the matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix v)
    | V.null v  = Matrix V.empty
    | otherwise = Matrix $ V.fromList $ map V.fromList $ L.transpose $ map V.toList $ V.toList v

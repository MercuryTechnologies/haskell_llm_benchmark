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
import Data.Maybe (fromMaybe)
import qualified Data.List as L

-- Matrix is represented as a vector of vectors (rows) with shape information
data Matrix a = Matrix
    { matrixData :: Vector (Vector a)  -- Rows of data
    , matrixShape :: (Int, Int)        -- (rows, cols)
    } deriving (Eq)

instance Show a => Show (Matrix a) where
    show matrix = unlines $ map (unwords . map show . V.toList) $ V.toList $ matrixData matrix

-- Get the number of columns in the matrix
cols :: Matrix a -> Int
cols = snd . matrixShape

-- Get a specific column from the matrix
column :: Int -> Matrix a -> Vector a
column x matrix = V.map (\row -> row V.! (x - 1)) (matrixData matrix)

-- Flatten the matrix into a single vector
flatten :: Matrix a -> Vector a
flatten matrix = V.concatMap id (matrixData matrix)

-- Create a matrix from a list of lists
fromList :: [[a]] -> Matrix a
fromList [] = Matrix V.empty (0, 0)
fromList xss = 
    let rowVecs = V.fromList (map V.fromList xss)
        r = V.length rowVecs
        c = if r > 0 then V.length (rowVecs V.! 0) else 0
    in Matrix rowVecs (r, c)

-- Create a matrix from a string
fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read . words) $ lines xs

-- Reshape a matrix to new dimensions
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix =
    let flat = flatten matrix
        total = r * c
        -- Ensure we have enough elements, or cycle the existing ones
        paddedFlat = if V.length flat >= total
                     then flat
                     else V.generate total (\i -> flat V.! (i `mod` V.length flat))
        -- Create new rows
        newRows = V.generate r (\i -> 
            V.slice (i * c) c paddedFlat)
    in Matrix newRows (r, c)

-- Get a specific row from the matrix
row :: Int -> Matrix a -> Vector a
row x matrix = matrixData matrix V.! (x - 1)

-- Get the number of rows in the matrix
rows :: Matrix a -> Int
rows = fst . matrixShape

-- Get the shape (rows, cols) of the matrix
shape :: Matrix a -> (Int, Int)
shape = matrixShape

-- Transpose the matrix (rows become columns and vice versa)
transpose :: Matrix a -> Matrix a
transpose matrix =
    let (r, c) = matrixShape matrix
        transposed = V.generate c (\i -> column (i + 1) matrix)
    in Matrix transposed (c, r)

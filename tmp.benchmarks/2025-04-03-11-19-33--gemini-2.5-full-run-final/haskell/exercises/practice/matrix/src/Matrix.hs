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

-- Represent the matrix as a flat vector along with its dimensions (rows, cols).
-- This makes reshaping and flattening efficient.
data Matrix a = Matrix {
    nrows :: Int,
    ncols :: Int,
    mvector :: Vector a
} deriving (Eq, Show)

-- | Get the number of columns in the matrix.
cols :: Matrix a -> Int
cols = ncols

-- | Extract a specific column (1-indexed) as a vector.
-- Example: column 2 for the matrix:
-- 9 8 7
-- 5 3 2
-- 6 6 7
-- should return the vector [8, 3, 6]
column :: Int -> Matrix a -> Vector a
column c matrix
    | c <= 0 || c > ncols matrix = error "Column index out of bounds"
    | otherwise = let c0 = c - 1 -- Convert to 0-based index
                  in V.generate (nrows matrix) $ \r -> mvector matrix V.! (r * ncols matrix + c0)

-- | Flatten the matrix into a single vector, row by row.
flatten :: Matrix a -> Vector a
flatten = mvector

-- | Create a matrix from a list of lists.
-- Assumes the input is rectangular.
fromList :: [[a]] -> Matrix a
fromList [] = Matrix 0 0 V.empty
-- Use _ to ignore the unused tail of the list, fixing the warning.
fromList xss@(r:_) = Matrix {
    nrows = length xss,
    ncols = length r,
    mvector = V.fromList $ concat xss
}

-- | Create a matrix from a string representation.
-- Numbers are separated by spaces, rows by newlines.
fromString :: Read a => String -> Matrix a
fromString s = fromList $ map (map read . words) $ lines s

-- | Reshape the matrix into new dimensions.
-- The total number of elements must remain the same.
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix
    | r * c /= V.length (mvector matrix) = error "New shape must have the same number of elements"
    | otherwise = matrix { nrows = r, ncols = c }

-- | Extract a specific row (1-indexed) as a vector.
-- Example: row 1 for the matrix:
-- 9 8 7
-- 5 3 2
-- 6 6 7
-- should return the vector [9, 8, 7]
row :: Int -> Matrix a -> Vector a
row r matrix
    | r <= 0 || r > nrows matrix = error "Row index out of bounds"
    | otherwise = let r0 = r - 1 -- Convert to 0-based index
                  in V.slice (r0 * ncols matrix) (ncols matrix) (mvector matrix)

-- | Get the number of rows in the matrix.
rows :: Matrix a -> Int
rows = nrows

-- | Get the shape (rows, cols) of the matrix.
shape :: Matrix a -> (Int, Int)
shape matrix = (nrows matrix, ncols matrix)

-- | Transpose the matrix (swap rows and columns).
transpose :: Matrix a -> Matrix a
transpose matrix = Matrix {
    nrows = ncols matrix,
    ncols = nrows matrix,
    -- Generate the new vector by picking elements column by column from the original
    -- Indices i, r, c here are 0-based for vector generation logic.
    mvector = V.generate (V.length $ mvector matrix) $ \i ->
                let r_old = i `div` (nrows matrix) -- Calculate old row index (0-based)
                    c_old = i `mod` (nrows matrix) -- Calculate old col index (0-based)
                in mvector matrix V.! (c_old * ncols matrix + r_old) -- Access element at (c_old, r_old) in original vector
}


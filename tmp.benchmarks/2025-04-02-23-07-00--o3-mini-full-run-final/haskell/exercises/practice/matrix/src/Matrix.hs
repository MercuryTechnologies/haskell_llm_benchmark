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

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.List as L

newtype Matrix a = Matrix { getRows :: [[a]] } deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix []) = 0
cols (Matrix (r:_)) = length r

column :: Int -> Matrix a -> Vector a
column n (Matrix rows)
    | n < 1 || n > length (head rows) = error "column: index out of bounds"
    | otherwise = V.fromList [ row !! (n - 1) | row <- rows ]
  where
    row = id

flatten :: Matrix a -> Vector a
flatten (Matrix rows) = V.fromList (concat rows)

fromList :: [[a]] -> Matrix a
fromList rows = Matrix rows

fromString :: Read a => String -> Matrix a
fromString str = Matrix $ map (map read . words) (lines str)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) m
    | newRows * newCols /= total = error "reshape: dimensions do not match"
    | otherwise = Matrix (chunk newCols flat)
  where
    flat = V.toList (flatten m)
    total = length flat
    chunk _ [] = []
    chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

row :: Int -> Matrix a -> Vector a
row n (Matrix rows)
    | n < 1 || n > length rows = error "row: index out of bounds"
    | otherwise = V.fromList (rows !! (n - 1))

rows :: Matrix a -> Int
rows (Matrix rows) = length rows

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose (Matrix rows) = Matrix (L.transpose rows)

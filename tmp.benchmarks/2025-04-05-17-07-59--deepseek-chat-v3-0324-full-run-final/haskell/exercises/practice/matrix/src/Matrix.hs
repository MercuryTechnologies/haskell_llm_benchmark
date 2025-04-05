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
import qualified Data.List as L

newtype Matrix a = Matrix { getMatrix :: [[a]] } deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m) = if null m then 0 else length (head m)

column :: Int -> Matrix a -> V.Vector a
column x (Matrix m) = V.fromList (map (!! x) m)

flatten :: Matrix a -> V.Vector a
flatten (Matrix m) = V.fromList (concat m)

fromList :: [[a]] -> Matrix a
fromList = Matrix

fromString :: Read a => String -> Matrix a
fromString = Matrix . map (map read . words) . lines

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix m) = 
    let flat = concat m
    in Matrix $ take r $ iterate (drop c) flat

row :: Int -> Matrix a -> V.Vector a
row x (Matrix m) = V.fromList (m !! x)

rows :: Matrix a -> Int
rows (Matrix m) = length m

shape :: Matrix a -> (Int, Int)
shape (Matrix m) = (length m, if null m then 0 else length (head m))

transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix (L.transpose m)

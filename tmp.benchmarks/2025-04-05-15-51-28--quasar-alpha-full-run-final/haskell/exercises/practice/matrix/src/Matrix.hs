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
import qualified Data.List as List

data Matrix a = Matrix (Vector (Vector a))
    deriving (Eq, Show)

fromList :: [[a]] -> Matrix a
fromList xs = Matrix $ V.fromList (map V.fromList xs)

fromString :: Read a => String -> Matrix a
fromString s = fromList $ map (map read . words) (lines s)

rows :: Matrix a -> Int
rows (Matrix v) = V.length v

cols :: Matrix a -> Int
cols (Matrix v)
    | V.null v  = 0
    | otherwise = V.length (V.head v)

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

row :: Int -> Matrix a -> Vector a
row i (Matrix v) = v V.! (i - 1)

column :: Int -> Matrix a -> Vector a
column j (Matrix v) = V.map (\r -> r V.! (j - 1)) v

flatten :: Matrix a -> Vector a
flatten (Matrix v) = V.concat (V.toList v)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m
    | r * c /= total = error "Invalid reshape dimensions"
    | otherwise = fromList $ chunk c (V.toList (flatten m))
  where
    total = rows m * cols m
    chunk _ [] = []
    chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

transpose :: Matrix a -> Matrix a
transpose (Matrix v) = fromList $ List.transpose (map V.toList (V.toList v))

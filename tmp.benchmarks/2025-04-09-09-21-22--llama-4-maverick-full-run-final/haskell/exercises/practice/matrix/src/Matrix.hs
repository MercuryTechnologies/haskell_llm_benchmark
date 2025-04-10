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

import Data.Vector (Vector, fromList, toList, (!))
import qualified Data.Vector as V
import Data.List (transpose)

newtype Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

fromList :: [[a]] -> Matrix a
fromList = Matrix . V.fromList . map V.fromList

fromString :: Read a => String -> Matrix a
fromString = Matrix.fromList . map (map read . words) . lines

rows :: Matrix a -> Int
rows (Matrix matrix) = V.length matrix

cols :: Matrix a -> Int
cols (Matrix matrix) = maybe 0 V.length (headMay matrix)

row :: Int -> Matrix a -> Vector a
row i (Matrix matrix) = matrix ! (i - 1)

column :: Int -> Matrix a -> Vector a
column j (Matrix matrix) = V.map (! (j - 1)) matrix

flatten :: Matrix a -> Vector a
flatten (Matrix matrix) = V.concat matrix

shape :: Matrix a -> (Int, Int)
shape matrix = (rows matrix, cols matrix)

transpose :: Matrix a -> Matrix a
transpose (Matrix matrix) = Matrix (V.fromList (transpose (toList (V.map toList matrix))))

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) matrix
  | r * c /= rows matrix * cols matrix = error "Invalid reshape dimensions"
  | otherwise = Matrix.fromList . chunks c . toList $ flatten matrix
  where
    chunks _ [] = []
    chunks n xs = take n xs : chunks n (drop n xs)
    headMay :: Vector a -> Maybe a
    headMay vec
      | V.null vec = Nothing
      | otherwise = Just (V.head vec)

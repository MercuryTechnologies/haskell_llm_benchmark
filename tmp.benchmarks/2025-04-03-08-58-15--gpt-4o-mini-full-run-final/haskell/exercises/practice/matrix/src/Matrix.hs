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

import Data.Vector (Vector, fromList)
import qualified Data.Vector as V

data Matrix a = Matrix [[a]] deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix xss) = length (head xss)

column :: Int -> Matrix a -> Vector a
column x (Matrix xss) = fromList [row !! x | row <- xss]

flatten :: Matrix a -> Vector a
flatten (Matrix xss) = fromList (concat xss)

fromList :: [[a]] -> Matrix a
fromList xss = Matrix xss

fromString :: Read a => String -> Matrix a
fromString xs = fromList (map (map read . words) (lines xs))

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (newRows, newCols) (Matrix xss) = Matrix (take newRows (map (take newCols) xss))

row :: Int -> Matrix a -> Vector a
row x (Matrix xss) = fromList (xss !! x)

rows :: Matrix a -> Int
rows (Matrix xss) = length xss

shape :: Matrix a -> (Int, Int)
shape (Matrix xss) = (length xss, length (head xss))

transpose :: Matrix a -> Matrix a
transpose (Matrix xss) = Matrix (getColumns xss)
  where
    getColumns [] = []
    getColumns xss = map (map head) (takeWhile (not . null) (iterate (map tail) xss))

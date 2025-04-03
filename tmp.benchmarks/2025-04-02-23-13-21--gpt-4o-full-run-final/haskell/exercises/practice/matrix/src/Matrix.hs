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

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data Matrix a = Matrix (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix m) = if V.null m then 0 else V.length (m ! 0)

column :: Int -> Matrix a -> Vector a
column x (Matrix m) = V.map (! x) m

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat (V.toList m)

fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.fromList (map V.fromList xss))

fromString :: Read a => String -> Matrix a
fromString xs = fromList $ map (map read . words) (lines xs)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix m) = Matrix $ V.generate r (\i -> V.slice (i * c) c (flatten (Matrix m)))

row :: Int -> Matrix a -> Vector a
row x (Matrix m) = m ! x

rows :: Matrix a -> Int
rows (Matrix m) = V.length m

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix $ V.generate (cols (Matrix m)) (\i -> column i (Matrix m))

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

data Matrix a = M (Vector (Vector a)) deriving (Eq, Show)

cols :: Matrix a -> Int
cols (M v) =
  if V.null v
    then 0
    else V.length (v V.! 0)

column :: Int -> Matrix a -> Vector a
column j (M v) =
  V.map (\row -> row V.! j) v

flatten :: Matrix a -> Vector a
flatten (M v) =
  V.concat (V.toList v)

fromList :: [[a]] -> Matrix a
fromList xss =
  M (V.fromList (map V.fromList xss))

fromString :: Read a => String -> Matrix a
fromString str =
  fromList $ map (map read . words) (lines str)

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) m =
  let flatVals = flatten m
      chunks = chunk c flatVals
  in M $ V.fromList chunks
  where
    chunk :: Int -> Vector a -> [Vector a]
    chunk n xs
      | V.null xs = []
      | otherwise =
          let (first, rest) = V.splitAt n xs
          in first : chunk n rest

row :: Int -> Matrix a -> Vector a
row i (M v) =
  v V.! i

rows :: Matrix a -> Int
rows (M v) =
  V.length v

shape :: Matrix a -> (Int, Int)
shape m =
  (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose (M v)
  | V.null v = M V.empty
  | otherwise =
      let asLists = map V.toList (V.toList v)
          transposed = L.transpose asLists
      in fromList transposed

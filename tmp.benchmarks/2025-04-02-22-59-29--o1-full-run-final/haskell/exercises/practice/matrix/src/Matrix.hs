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

data Matrix a = Matrix
  { mRows :: Int
  , mCols :: Int
  , mData :: Vector a
  } deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix _ c _) = c

-- Now using 1-based indexing for column index:
column :: Int -> Matrix a -> Vector a
column i (Matrix r c d) =
  V.generate r (\rowIndex -> d V.! (rowIndex * c + (i - 1)))

flatten :: Matrix a -> Vector a
flatten (Matrix _ _ d) = d

fromList :: [[a]] -> Matrix a
fromList xss =
  let r = length xss
      c = length (head xss)
      d = V.fromList (concat xss)
  in Matrix r c d

fromString :: Read a => String -> Matrix a
fromString s =
  let rowsOfStr = lines s
      parseRow line = map read (words line)
      xss = map parseRow rowsOfStr
  in fromList xss

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix _ _ d) =
  Matrix r c d

-- Now using 1-based indexing for row index:
row :: Int -> Matrix a -> Vector a
row i (Matrix _ c d) =
  V.slice ((i - 1) * c) c d

rows :: Matrix a -> Int
rows (Matrix r _ _) = r

shape :: Matrix a -> (Int, Int)
shape (Matrix r c _) = (r, c)

transpose :: Matrix a -> Matrix a
transpose (Matrix r c d) =
  let d' = V.generate (r * c) (\idx ->
             let oldRow = idx `div` r
                 oldCol = idx `mod` r
             in d V.! (oldCol * c + oldRow))
  in Matrix c r d'

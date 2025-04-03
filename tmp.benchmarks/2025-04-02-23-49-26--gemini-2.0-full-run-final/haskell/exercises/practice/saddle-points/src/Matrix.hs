module Matrix (saddlePoints) where

import Data.Array (Array, bounds, indices, (!))

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix =
  let
    ( (rowMin, colMin), (rowMax, colMax) ) = bounds matrix
    rowMaxima =
      [ (row, col)
        | row <- [rowMin .. rowMax],
          let rowVals = [matrix ! (row, c) | c <- [colMin .. colMax]],
          let maxVal = maximum rowVals,
          col <- [colMin .. colMax],
          matrix ! (row, col) == maxVal
      ]
    colMinima =
      [ (row, col)
        | col <- [colMin .. colMax],
          let colVals = [matrix ! (r, col) | r <- [rowMin .. rowMax]],
          let minVal = minimum colVals,
          row <- [rowMin .. rowMax],
          matrix ! (row, col) == minVal
      ]
  in
    filter (`elem` colMinima) rowMaxima

module Spiral (spiral) where

-- | Generates a square matrix of natural numbers from 1..(size*size),
--   laid out in a clockwise spiral pattern.
spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | otherwise = let (finalMatrix, _) = fillSpiral (replicate n (replicate n 0))
                                                  1
                                                  0 (n - 1)
                                                  0 (n - 1)
                in finalMatrix
  where
    fillSpiral :: [[Int]] -> Int
               -> Int -> Int
               -> Int -> Int
               -> ([[Int]], Int)
    fillSpiral matrix current left right top bottom
      | left > right || top > bottom = (matrix, current)
      | otherwise =
          -- 1) Fill the top row left to right
          let (matrix1, next1) = fillRowLeftToRight matrix current top (left, right)
              -- 2) Fill the right column top+1 to bottom
              (matrix2, next2) = fillColTopToBottom matrix1 next1 right (top + 1, bottom)
              -- 3) If there's a remaining row, fill the bottom row right-1 to left
              (matrix3, next3) =
                if top < bottom
                  then fillRowRightToLeft matrix2 next2 bottom (right - 1, left)
                  else (matrix2, next2)
              -- 4) If there's a remaining column, fill the left column bottom-1 to top+1
              (matrix4, next4) =
                if left < right
                  then fillColBottomToTop matrix3 next3 left (bottom - 1, top + 1)
                  else (matrix3, next3)
          in fillSpiral matrix4 next4 (left + 1) (right - 1) (top + 1) (bottom - 1)

    -- Fill a row left to right
    fillRowLeftToRight :: [[Int]] -> Int -> Int -> (Int, Int) -> ([[Int]], Int)
    fillRowLeftToRight mat start row (c1, c2) =
      foldl (\(m, num) c -> (updateMatrix m row c num, num + 1))
            (mat, start)
            [c1..c2]

    -- Fill a row right to left
    fillRowRightToLeft :: [[Int]] -> Int -> Int -> (Int, Int) -> ([[Int]], Int)
    fillRowRightToLeft mat start row (c1, c2) =
      foldl (\(m, num) c -> (updateMatrix m row c num, num + 1))
            (mat, start)
            [c1, c1 - 1 .. c2]

    -- Fill a column top to bottom
    fillColTopToBottom :: [[Int]] -> Int -> Int -> (Int, Int) -> ([[Int]], Int)
    fillColTopToBottom mat start col (r1, r2) =
      foldl (\(m, num) r -> (updateMatrix m r col num, num + 1))
            (mat, start)
            [r1..r2]

    -- Fill a column bottom to top
    fillColBottomToTop :: [[Int]] -> Int -> Int -> (Int, Int) -> ([[Int]], Int)
    fillColBottomToTop mat start col (r1, r2) =
      foldl (\(m, num) r -> (updateMatrix m r col num, num + 1))
            (mat, start)
            [r1, r1 - 1 .. r2]

    -- Update a single cell in the matrix
    updateMatrix :: [[Int]] -> Int -> Int -> Int -> [[Int]]
    updateMatrix mat r c val =
      take r mat
      ++ [updateRow (mat !! r) c val]
      ++ drop (r + 1) mat

    -- Update a single element in a row
    updateRow :: [Int] -> Int -> Int -> [Int]
    updateRow row idx val =
      take idx row ++ [val] ++ drop (idx + 1) row

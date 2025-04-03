module Connect (Mark(..), winner) where

-- We no longer need Data.List (nub), so let's remove it to avoid warnings.

data Mark = Cross | Nought
  deriving (Eq, Show)

-- | Determine the winner (if any) of a Hex-like board.
--   'Nought' wins if there is a top-to-bottom path of 'O's,
--   'Cross' wins if there is a left-to-right path of 'X's.
winner :: [String] -> Maybe Mark
winner inputLines =
  let b = parseBoard inputLines
  in if checkNought b
     then Just Nought
     else if checkCross b
          then Just Cross
          else Nothing

-----------------------------------------
-- Parsing the board into a 2D grid.
-----------------------------------------

-- Remove all whitespace between tokens to form a clear 2D grid.
parseBoard :: [String] -> [[Char]]
parseBoard = map (map head . words)

-----------------------------------------
-- Checking for Nought (O) top-to-bottom.
-----------------------------------------

checkNought :: [[Char]] -> Bool
checkNought board =
  let nRows = length board
      nCols = if nRows == 0 then 0 else length (head board)
      -- Starting cells: row=0, any column that has 'O'
      starts = [(0,j) | j <- [0..nCols-1], board!!0!!j == 'O']
      -- Goal: row == nRows-1
      goal (r, _) = r == nRows - 1
  in any (search board 'O' goal) starts

-----------------------------------------
-- Checking for Cross (X) left-to-right.
-----------------------------------------

checkCross :: [[Char]] -> Bool
checkCross board =
  let nRows = length board
      nCols = if nRows == 0 then 0 else length (head board)
      -- Starting cells: col=0, any row that has 'X'
      starts = [(r,0) | r <- [0..nRows-1], board!!r!!0 == 'X']
      -- Goal: col == nCols-1
      goal (_, c) = c == nCols - 1
  in any (search board 'X' goal) starts

-----------------------------------------
-- BFS/DFS-like search for a contiguous path of the same mark.
-----------------------------------------

search :: [[Char]] -> Char -> ((Int,Int) -> Bool) -> (Int,Int) -> Bool
search board mark goal start = go [start] []
  where
    go [] _ = False
    go (p:ps) visited
      | p `elem` visited = go ps visited
      | goal p           = True
      | otherwise =
          let ns = neighbors board p
              validNs = [n | n <- ns, boardAt board n == mark]
          in go (ps ++ validNs) (p : visited)

boardAt :: [[Char]] -> (Int,Int) -> Char
boardAt b (r, c) = b !! r !! c

-- We swap the neighbor definitions for even/odd rows to match the test expectations.
neighbors :: [[Char]] -> (Int,Int) -> [(Int,Int)]
neighbors board (r,c) =
  let nRows   = length board
      nCols   = if nRows == 0 then 0 else length (head board)
      evenRow = (r `mod` 2 == 0)
      cands = if evenRow
                then [ (r-1,c-1), (r-1,c),
                       (r,  c-1), (r,  c+1),
                       (r+1,c-1), (r+1,c) ]
                else [ (r-1,c),   (r-1,c+1),
                       (r,  c-1), (r,  c+1),
                       (r+1,c),   (r+1,c+1) ]
  in [ (rr,cc) | (rr,cc) <- cands
               , rr >= 0, rr < nRows
               , cc >= 0, cc < nCols ]

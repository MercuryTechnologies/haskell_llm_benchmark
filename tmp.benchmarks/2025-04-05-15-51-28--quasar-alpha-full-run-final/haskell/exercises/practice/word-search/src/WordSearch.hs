module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord word)) wordList
  where
    numRows = length grid
    numCols = if null grid then 0 else length (head grid)

    -- All 8 directions: (dx, dy)
    directions = [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,-1), (1,-1), (-1,1)]

    -- Check if a position is within grid bounds
    inBounds x y = x >= 0 && y >= 0 && x < numCols && y < numRows

    -- Try to find the word starting from (x,y) in direction (dx,dy)
    matchFrom x y dx dy word =
      let positions = take (length word) [(x + i*dx, y + i*dy) | i <- [0..]]
      in if all (\(cx, cy) -> inBounds cx cy) positions &&
            and [ (grid !! cy) !! cx == c | ((cx, cy), c) <- zip positions word ]
         then Just (CharPos x y, CharPos (x + (length word -1)*dx) (y + (length word -1)*dy))
         else Nothing

    -- Find the word anywhere in the grid
    findWord word = 
      let matches = [ WordPos startPos endPos
                    | y <- [0..numRows-1]
                    , x <- [0..numCols-1]
                    , (dx, dy) <- directions
                    , Just (startPos, endPos) <- [matchFrom x y dx dy word]
                    ]
      in case matches of
           -- select the match with the smallest (col,row) start position (1-based)
           [] -> Nothing
           _  -> Just $ minimumByPos matches

    minimumByPos :: [WordPos] -> WordPos
    minimumByPos = foldr1 minPos

    minPos :: WordPos -> WordPos -> WordPos
    minPos wp1 wp2 =
      let CharPos c1 r1 = start wp1
          CharPos c2 r2 = start wp2
      in if (r1, c1) < (r2, c2) then wp1 else wp2

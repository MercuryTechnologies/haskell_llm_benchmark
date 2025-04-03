module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord w)) wordList
  where
    nrows = length grid
    ncols = if null grid then 0 else length (head grid)

    directions :: [(Int, Int)]
    directions =
      [ (0, 1)   -- left to right
      , (0, -1)  -- right to left
      , (1, 0)   -- top to bottom
      , (-1, 0)  -- bottom to top
      , (1, 1)   -- diagonal down-right
      , (1, -1)  -- diagonal down-left
      , (-1, 1)  -- diagonal up-right
      , (-1, -1) -- diagonal up-left
      ]

    charAt :: Int -> Int -> Char
    charAt r c = (grid !! r) !! c

    checkWord :: String -> Int -> Int -> Int -> Int -> Maybe WordPos
    checkWord w r c dr dc
      | any outOfBounds positions = Nothing
      | map (uncurry charAt) positions == w =
          Just WordPos
            { start = CharPos (c + 1) (r + 1)
            , end   = CharPos ((c + 1) + (length w - 1) * dc)
                             ((r + 1) + (length w - 1) * dr)
            }
      | otherwise = Nothing
      where
        positions = zip
          [r, r + dr .. r + dr * (length w - 1)]
          [c, c + dc .. c + dc * (length w - 1)]
        outOfBounds (rr, cc) =
          rr < 0 || rr >= nrows || cc < 0 || cc >= ncols

    findWord :: String -> Maybe WordPos
    findWord w =
      case [ wp
           | r <- [0..nrows-1]
           , c <- [0..ncols-1]
           , (dr, dc) <- directions
           , Just wp <- [checkWord w r c dr dc]
           ] of
        []    -> Nothing
        (x:_) -> Just x

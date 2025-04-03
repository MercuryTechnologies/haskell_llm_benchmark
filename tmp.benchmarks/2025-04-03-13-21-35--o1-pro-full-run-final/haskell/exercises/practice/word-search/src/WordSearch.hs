module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordsToFind = map (\w -> (w, findWord grid w)) wordsToFind

findWord :: [String] -> String -> Maybe WordPos
findWord grid word
  | null grid || null (head grid) || null word = Nothing
  | otherwise = go 0
  where
    rows = length grid
    cols = length (head grid)

    go i
      | i >= rows * cols = Nothing
      | otherwise =
          let (r, c) = i `divMod` cols
          in case checkAllDirections grid r c directions word of
               Nothing -> go (i + 1)
               Just wp -> Just wp

    directions :: [(Int, Int)]
    directions = [ (0, 1)    -- left-to-right
                 , (0, -1)   -- right-to-left
                 , (1, 0)    -- top-to-bottom
                 , (-1, 0)   -- bottom-to-top
                 , (1, 1)    -- diagonal down-right
                 , (-1, -1)  -- diagonal up-left
                 , (1, -1)   -- diagonal down-left
                 , (-1, 1)   -- diagonal up-right
                 ]

checkAllDirections :: [String] -> Int -> Int -> [(Int, Int)] -> String -> Maybe WordPos
checkAllDirections _ _ _ [] _ = Nothing
checkAllDirections grid r c (dir:ds) word =
  case checkDirection grid r c dir word of
    Just wp -> Just wp
    Nothing -> checkAllDirections grid r c ds word

checkDirection :: [String] -> Int -> Int -> (Int, Int) -> String -> Maybe WordPos
checkDirection grid r c (dr, dc) word =
  if all inBounds [0..(len - 1)] && all matchChar [0..(len - 1)]
    then Just (WordPos
                  (CharPos (c + 1) (r + 1))
                  (CharPos (c + dc * (len - 1) + 1)
                           (r + dr * (len - 1) + 1)))
    else Nothing
  where
    rows = length grid
    cols = length (head grid)
    len  = length word

    inBounds i =
      let rr = r + dr * i
          cc = c + dc * i
      in rr >= 0 && rr < rows && cc >= 0 && cc < cols

    matchChar i =
      let rr = r + dr * i
          cc = c + dc * i
      in (grid !! rr) !! cc == word !! i

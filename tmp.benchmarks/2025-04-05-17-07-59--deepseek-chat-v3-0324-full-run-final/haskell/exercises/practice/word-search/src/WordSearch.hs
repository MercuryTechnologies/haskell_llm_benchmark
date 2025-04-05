module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (findWord grid) wordList

findWord :: [String] -> String -> (String, Maybe WordPos)
findWord grid word = 
    case findWordPosition grid word of
        Nothing -> (word, Nothing)
        Just (startPos, endPos) -> (word, Just $ WordPos startPos endPos)

findWordPosition :: [String] -> String -> Maybe (CharPos, CharPos)
findWordPosition grid word = 
    case [ (startPos, endPos) 
         | y <- [0..rows-1]
         , x <- [0..cols-1]
         , let startPos = CharPos x y
         , Just endPos <- [checkAllDirections grid word startPos]
         ] of
        [] -> Nothing
        (x:_) -> Just x
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)

checkAllDirections :: [String] -> String -> CharPos -> Maybe CharPos
checkAllDirections grid word (CharPos x y) =
    case filter (/= Nothing) $ map checkDirection directions of
        (endPos:_) -> endPos
        _ -> Nothing
  where
    directions = [(dx, dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx, dy) /= (0,0)]
    checkDirection (dx, dy) = checkDirection' word x y dx dy
    
    checkDirection' [] _ _ _ _ = Nothing
    checkDirection' (c:cs) x' y' dx dy
        | x' < 0 || y' < 0 || y' >= rows || x' >= cols = Nothing
        | grid !! y' !! x' /= c = Nothing
        | null cs = Just (CharPos x' y')
        | otherwise = checkDirection' cs (x'+dx) (y'+dy) dx dy

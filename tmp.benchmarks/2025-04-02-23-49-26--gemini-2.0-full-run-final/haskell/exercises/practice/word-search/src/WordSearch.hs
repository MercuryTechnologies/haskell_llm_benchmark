module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord word grid)) wordList

findWord :: String -> [String] -> Maybe WordPos
findWord word grid =
  case findWord' word grid of
    [] -> Nothing
    (pos:_) -> Just pos

findWord' :: String -> [String] -> [WordPos]
findWord' word grid =
  concatMap (\(r, row) -> concatMap (\(c, _) -> searchFrom (CharPos c r) word grid) (zip [0..] row)) (zip [0..] grid)

searchFrom :: CharPos -> String -> [String] -> [WordPos]
searchFrom startPos word grid =
  let
    rows = length grid
    cols = length (head grid)
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    found = filter isJust $ map (\(rowDir, colDir) -> searchDirection startPos word grid rowDir colDir rows cols) directions
  in
    map fromJust found

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Prelude.fromJust: Nothing"

searchDirection :: CharPos -> String -> [String] -> Int -> Int -> Int -> Int -> Maybe WordPos
searchDirection startPos word grid rowDir colDir rows cols =
  let
    checkChar :: CharPos -> Int -> Bool
    checkChar (CharPos c r) i =
      r >= 0 && r < rows && c >= 0 && c < cols && (grid !! r !! c) == (word !! i)

    go :: CharPos -> Int -> Maybe CharPos
    go currentPos i
      | i == length word = Just currentPos
      | checkChar currentPos i = go (CharPos (col currentPos + colDir) (row currentPos + rowDir)) (i + 1)
      | otherwise = Nothing

  in
    case go startPos 0 of
      Just endPos -> Just (WordPos startPos (CharPos (col endPos - colDir) (row endPos - rowDir)))
      Nothing -> Nothing

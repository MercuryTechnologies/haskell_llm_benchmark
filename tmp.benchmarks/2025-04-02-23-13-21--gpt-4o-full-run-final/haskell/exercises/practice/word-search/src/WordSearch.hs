module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = 
    let directions = [(1, 0), (0, 1), (1, 1), (1, -1), (-1, 0), (0, -1), (-1, -1), (-1, 1)]
        positions = [(r, c) | r <- [0..length grid - 1], c <- [0..length (head grid) - 1]]
    in case [(startPos, endPos) | (row, col) <- positions, (dr, dc) <- directions, 
                                  let startPos = CharPos col row,
                                  let endPos = findEnd grid word (row, col) (dr, dc),
                                  endPos /= startPos] of
        ((startPos, endPos):_) -> Just (WordPos startPos endPos)
        [] -> Nothing

findEnd :: [String] -> String -> (Int, Int) -> (Int, Int) -> CharPos
findEnd grid word (row, col) (dr, dc) = 
    let positions = take (length word) $ iterate (\(r, c) -> (r + dr, c + dc)) (row, col)
        chars = map (\(r, c) -> if r >= 0 && r < length grid && c >= 0 && c < length (head grid) 
                                then Just (grid !! r !! c) 
                                else Nothing) positions
    in if map Just word == take (length word) chars
       then CharPos (col + (length word - 1) * dc) (row + (length word - 1) * dr)
       else CharPos col row

module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = 
  case searchAllDirections grid word of
    [] -> Nothing
    (pos:_) -> Just pos

searchAllDirections :: [String] -> String -> [WordPos]
searchAllDirections grid word =
  concatMap (\f -> f grid word) [searchHorizontal, searchVertical, searchDiagonal]

-- Search horizontally (left-to-right and right-to-left)
searchHorizontal :: [String] -> String -> [WordPos]
searchHorizontal grid word = 
  let height = length grid
      width = if height > 0 then length (head grid) else 0
      -- Left to right
      ltr = concatMap (\r -> 
              map (\c -> WordPos 
                          (CharPos c r) 
                          (CharPos (c + length word - 1) r))
              (findSubstring word (grid !! r) 0)) [0..height-1]
      -- Right to left
      rtl = concatMap (\r -> 
              map (\c -> WordPos 
                          (CharPos (c + length word - 1) r) 
                          (CharPos c r))
              (findSubstring (reverse word) (grid !! r) 0)) [0..height-1]
  in filter (\(WordPos (CharPos c1 _) (CharPos c2 _)) -> 
              c1 >= 0 && c1 < width && c2 >= 0 && c2 < width) (ltr ++ rtl)

-- Search vertically (top-to-bottom and bottom-to-top)
searchVertical :: [String] -> String -> [WordPos]
searchVertical grid word =
  let height = length grid
      width = if height > 0 then length (head grid) else 0
      transposed = transpose grid
      -- Top to bottom
      ttb = concatMap (\c -> 
              map (\r -> WordPos 
                          (CharPos c r) 
                          (CharPos c (r + length word - 1)))
              (findSubstring word (transposed !! c) 0)) [0..width-1]
      -- Bottom to top
      btt = concatMap (\c -> 
              map (\r -> WordPos 
                          (CharPos c (r + length word - 1)) 
                          (CharPos c r))
              (findSubstring (reverse word) (transposed !! c) 0)) [0..width-1]
  in filter (\(WordPos (CharPos _ r1) (CharPos _ r2)) -> 
              r1 >= 0 && r1 < height && r2 >= 0 && r2 < height) (ttb ++ btt)

-- Search diagonally (all four diagonal directions)
searchDiagonal :: [String] -> String -> [WordPos]
searchDiagonal grid word =
  let height = length grid
      width = if height > 0 then length (head grid) else 0
      -- Get all possible diagonal lines
      diagonals1 = getDiagonals grid -- top-left to bottom-right
      diagonals2 = getDiagonalsReverse grid -- top-right to bottom-left
      
      -- Search in diagonals (top-left to bottom-right)
      d1 = concatMap (\(startPos, diag) ->
              map (\idx -> 
                let offset = length word - 1
                    (sc, sr) = startPos
                in WordPos (CharPos (sc + idx) (sr + idx)) 
                           (CharPos (sc + idx + offset - 1) (sr + idx + offset - 1)))
              (findSubstring word diag 0)) diagonals1
              
      -- Search in reverse diagonals (top-right to bottom-left)
      d2 = concatMap (\(startPos, diag) ->
              map (\idx -> 
                let offset = length word - 1
                    (sc, sr) = startPos
                in WordPos (CharPos (sc - idx) (sr + idx)) 
                           (CharPos (sc - idx - offset + 1) (sr + idx + offset - 1)))
              (findSubstring word diag 0)) diagonals2
              
      -- Search in reverse direction for both diagonal types
      d3 = concatMap (\(startPos, diag) ->
              map (\idx -> 
                let offset = length word - 1
                    (sc, sr) = startPos
                in WordPos (CharPos (sc + idx + offset - 1) (sr + idx + offset - 1))
                           (CharPos (sc + idx) (sr + idx)))
              (findSubstring (reverse word) diag 0)) diagonals1
              
      d4 = concatMap (\(startPos, diag) ->
              map (\idx -> 
                let offset = length word - 1
                    (sc, sr) = startPos
                in WordPos (CharPos (sc - idx - offset + 1) (sr + idx + offset - 1))
                           (CharPos (sc - idx) (sr + idx)))
              (findSubstring (reverse word) diag 0)) diagonals2
              
  in filter (\(WordPos (CharPos c1 r1) (CharPos c2 r2)) -> 
              c1 >= 0 && c1 < width && c2 >= 0 && c2 < width && 
              r1 >= 0 && r1 < height && r2 >= 0 && r2 < height) 
            (d1 ++ d2 ++ d3 ++ d4)

-- Get all diagonal lines from top-left to bottom-right
getDiagonals :: [String] -> [((Int, Int), String)]
getDiagonals grid =
  let height = length grid
      width = if height > 0 then length (head grid) else 0
      -- Diagonals starting from first column
      fromFirstCol = [(0, r) | r <- [0..height-1]]
      -- Diagonals starting from first row (excluding 0,0 which is already in fromFirstCol)
      fromFirstRow = [(c, 0) | c <- [1..width-1]]
      -- Get diagonal starting at position (c, r)
      getDiagonal (c, r) = 
        let positions = takeWhile (\(x, y) -> x < width && y < height) 
                                  [(c+i, r+i) | i <- [0..]]
        in ((c, r), [grid !! y !! x | (x, y) <- positions])
  in map getDiagonal (fromFirstCol ++ fromFirstRow)

-- Get all diagonal lines from top-right to bottom-left
getDiagonalsReverse :: [String] -> [((Int, Int), String)]
getDiagonalsReverse grid =
  let height = length grid
      width = if height > 0 then length (head grid) else 0
      -- Diagonals starting from last column
      fromLastCol = [(width-1, r) | r <- [0..height-1]]
      -- Diagonals starting from first row (excluding width-1,0 which is already in fromLastCol)
      fromFirstRow = [(c, 0) | c <- [0..width-2]]
      -- Get diagonal starting at position (c, r)
      getDiagonal (c, r) = 
        let positions = takeWhile (\(x, y) -> x >= 0 && y < height) 
                                  [(c-i, r+i) | i <- [0..]]
        in ((c, r), [grid !! y !! x | (x, y) <- positions])
  in map getDiagonal (fromLastCol ++ fromFirstRow)

-- Find all occurrences of a substring in a string
findSubstring :: String -> String -> Int -> [Int]
findSubstring needle haystack startIdx
  | startIdx + length needle > length haystack = []
  | take (length needle) (drop startIdx haystack) == needle = 
      startIdx : findSubstring needle haystack (startIdx + 1)
  | otherwise = findSubstring needle haystack (startIdx + 1)

-- Transpose a grid (convert rows to columns)
transpose :: [String] -> [String]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)

module Queens (boardString, canAttack) where

-- Function to check if two queens can attack each other
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) = r1 == r2 || c1 == c2 || abs (r1 - r2) == abs (c1 - c2)

-- Function to generate the string representation of the chessboard
boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [generateRow r white black | r <- [0..7]]

-- Helper function to generate a row of the chessboard
generateRow :: Int -> Maybe (Int, Int) -> Maybe (Int, Int) -> String
generateRow r white black = unwords [ [getSquare r c white black] | c <- [0..7]]

-- Helper function to determine the character for a square
getSquare :: Int -> Int -> Maybe (Int, Int) -> Maybe (Int, Int) -> Char
getSquare r c (Just w) b
  | (r, c) == w = 'W'
  | Just (r, c) == b = 'B'
  | otherwise = '_'
getSquare r c Nothing b
  | Just (r, c) == b = 'B'
  | otherwise = '_'

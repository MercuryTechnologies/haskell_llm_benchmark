module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | invalidBoard board = Impossible
  | hasWon 'X' board = WinX
  | hasWon 'O' board = WinO
  | isFull board = Draw
  | otherwise = Ongoing

-- Helper functions
invalidBoard :: [String] -> Bool
invalidBoard board = 
  let xCount = count 'X' board
      oCount = count 'O' board
      xWins = hasWon 'X' board
      oWins = hasWon 'O' board
  in xCount < oCount || xCount > oCount + 1 || (xWins && oWins) || (xWins && xCount == oCount) || (oWins && xCount > oCount)

count :: Char -> [String] -> Int
count c = sum . map (length . filter (== c))

hasWon :: Char -> [String] -> Bool
hasWon c [r0, r1, r2] =
  -- Check rows
  any (all (== c)) [r0, r1, r2] ||
  -- Check columns
  any (all (== c)) [[r0!!0, r1!!0, r2!!0], [r0!!1, r1!!1, r2!!1], [r0!!2, r1!!2, r2!!2]] ||
  -- Check diagonals
  all (== c) [r0!!0, r1!!1, r2!!2] || all (== c) [r0!!2, r1!!1, r2!!0]

isFull :: [String] -> Bool
isFull = all (notElem ' ')

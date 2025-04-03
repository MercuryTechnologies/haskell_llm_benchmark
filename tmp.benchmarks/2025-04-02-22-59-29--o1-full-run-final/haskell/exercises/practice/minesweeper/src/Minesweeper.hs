module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board =
  let height = length board
      width  = if height > 0 then length (head board) else 0
  in [ [ transform x y | x <- [0..(width-1)] ] | y <- [0..(height-1)] ]
  where
    transform :: Int -> Int -> Char
    transform x y =
      if (board !! y) !! x == '*'
        then '*'
        else
          let c = countNeighbors board x y
          in if c == 0
             then ' '
             else head (show c)

countNeighbors :: [String] -> Int -> Int -> Int
countNeighbors board x y =
  let neighborOffsets =
        [(-1, -1), (-1, 0), (-1, 1)
        ,(0, -1)          , (0, 1)
        ,(1, -1),  (1, 0),  (1, 1)]
      height = length board
      width  = if height > 0 then length (head board) else 0
      isMine nx ny
        | nx < 0 || nx >= width  = False
        | ny < 0 || ny >= height = False
        | otherwise              = (board !! ny) !! nx == '*'
  in sum [ 1 | (dx, dy) <- neighborOffsets, isMine (x+dx) (y+dy) ]

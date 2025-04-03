module Dominoes (chain) where

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes =
  case search [] dominoes of
    []      -> Nothing
    (s : _) -> Just s
  where
    -- search builds a chain using dominoes from the "remaining" list
    search :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
    search chainSoFar [] =
      if null chainSoFar || fst (head chainSoFar) == snd (last chainSoFar)
        then [chainSoFar]
        else []
    search chainSoFar remaining =
      concatMap nextMove (picks remaining)
      where
        nextMove :: ((Int, Int), [(Int, Int)]) -> [[(Int, Int)]]
        nextMove (domino, restRemaining) =
          [ solution
          | oriented <- orientations domino
          , null chainSoFar || snd (last chainSoFar) == fst oriented
          , solution <- search (chainSoFar ++ [oriented]) restRemaining
          ]
    
    -- Given a domino, return its possible orientations.
    orientations :: (Int, Int) -> [(Int, Int)]
    orientations (a, b)
      | a == b    = [(a, b)]
      | otherwise = [(a, b), (b, a)]
    
    -- Helper: picks returns each element with the remainder of the list.
    picks :: [a] -> [(a, [a])]
    picks []     = []
    picks (x:xs) = (x, xs) : [ (y, x:ys) | (y, ys) <- picks xs ]

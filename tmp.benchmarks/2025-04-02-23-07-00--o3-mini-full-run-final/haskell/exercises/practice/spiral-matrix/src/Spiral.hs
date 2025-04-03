module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n =
  let coords = spiralCoords 0 (n - 1) 0 (n - 1)
      mapping = [((r, c), num) | (num, (r, c)) <- zip [1..] coords]
  in [ [ lookupValue mapping (r, c) | c <- [0..n - 1] ] | r <- [0..n - 1] ]

lookupValue :: [((Int, Int), Int)] -> (Int, Int) -> Int
lookupValue [] pos = error ("No value found at position: " ++ show pos)
lookupValue (((r, c), v) : xs) pos@(r', c')
  | r == r' && c == c' = v
  | otherwise = lookupValue xs pos

spiralCoords :: Int -> Int -> Int -> Int -> [(Int, Int)]
spiralCoords top bottom left right
  | top > bottom || left > right = []
  | otherwise =
      let topRow = [ (top, c) | c <- [left..right] ]
          rightColumn = [ (r, right) | r <- [top + 1..bottom] ]
          bottomRow = if top < bottom then [ (bottom, c) | c <- reverse [left..right - 1] ] else []
          leftColumn = if left < right then [ (r, left) | r <- reverse [top + 1..bottom - 1] ] else []
      in topRow ++ rightColumn ++ bottomRow ++ leftColumn ++ spiralCoords (top + 1) (bottom - 1) (left + 1) (right - 1)

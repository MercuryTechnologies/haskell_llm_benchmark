module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs
  | n <= 0    = []
  | n > length xs = []
  | otherwise = map (map charToInt) $ take (length xs - n + 1) $ map (take n) $ tails xs
  where
    tails :: String -> [String]
    tails [] = []
    tails s@(_:xs) = s : tails xs

    charToInt :: Char -> Int
    charToInt c = read [c] :: Int

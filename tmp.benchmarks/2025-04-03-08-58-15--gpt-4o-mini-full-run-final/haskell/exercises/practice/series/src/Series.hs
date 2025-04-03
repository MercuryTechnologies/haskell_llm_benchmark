module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs
    | n > length xs = []
    | n == 0       = [[]]
    | n < 0        = []
    | otherwise     = map (map (\c -> read [c])) (take (length xs - n + 1) (map (take n) (tails xs)))

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

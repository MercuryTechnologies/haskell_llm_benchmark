module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
    | all (`elem` "01") xs = foldl (\acc (x, i) -> acc + (digitToInt x) * (2 ^ i)) 0 (zip (reverse xs) [0..])
    | otherwise = 0

digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt _   = error "Invalid binary digit"

module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

stringToInt :: String -> Either Error [Int]
stringToInt s = mapM charToInt s
  where
    charToInt c = case c of
      '0' -> Right 0
      '1' -> Right 1
      '2' -> Right 2
      '3' -> Right 3
      '4' -> Right 4
      '5' -> Right 5
      '6' -> Right 6
      '7' -> Right 7
      '8' -> Right 8
      '9' -> Right 9
      _   -> Left (InvalidDigit c)

seriesProduct :: [Int] -> Integer
seriesProduct = product . map toInteger

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size == 0 = Right 1
  | size > length digits = Left InvalidSpan
  | otherwise = do
    nums <- stringToInt digits
    let series = [take size (drop i nums) | i <- [0..(length nums - size)]]
    let products = map seriesProduct series
    return $ maximum products

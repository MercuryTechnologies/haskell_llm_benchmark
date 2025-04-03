module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | any (not . isDigit) digits = Left $ InvalidDigit (head $ filter (not . isDigit) digits)
  | otherwise = Right $ maximum $ map productOfSeries $ seriesOfSize size digits
  where
    seriesOfSize n = filter ((== n) . length) . map (take n) . tails
    productOfSeries = fromIntegral . product . map digitToInt

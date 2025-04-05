module Series (Error(..), largestProduct) where

import qualified Data.Char as Char (isDigit, digitToInt)
import Data.List (tails)
import Prelude hiding (product)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | any (not . Char.isDigit) digits = Left (InvalidDigit (head (filter (not . Char.isDigit) digits)))
  | size == 0 = Right 1
  | otherwise = Right $ maximum $ map seriesProduct $ series size digits

series :: Int -> String -> [[Integer]]
series size = filter ((== size) . length) . map (take size) . tails . map (toInteger . Char.digitToInt)

seriesProduct :: [Integer] -> Integer
seriesProduct = foldl (*) 1

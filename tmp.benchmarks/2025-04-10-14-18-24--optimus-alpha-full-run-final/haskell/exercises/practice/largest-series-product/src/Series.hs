module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)
import Control.Monad (foldM)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | size == 0 = Right 1
  | otherwise = do
      digitList <- traverse toDigit digits
      let series = filter ((== size) . length) $ map (take size) (tails digitList)
          products = map productOfSeries series
      return $ if null products then 1 else maximum products
  where
    toDigit c
      | isDigit c = Right (toInteger (digitToInt c))
      | otherwise = Left (InvalidDigit c)
    productOfSeries = foldl (*) 1

module Diamond (diamond) where

import qualified Data.Text as T
import           Data.Text (Text)

diamond :: Char -> Maybe [Text]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ map (T.pack . createRow) [0..n]
  where
    n = fromEnum c - fromEnum 'A'
    createRow i
      | i < n     = replicate (n - i) ' ' ++ [toEnum (fromEnum 'A' + i)] ++ replicate (i * 2 - 1) ' ' ++ [toEnum (fromEnum 'A' + i)] ++ replicate (n - i) ' '
      | i == n    = replicate n ' ' ++ [toEnum (fromEnum 'A' + n)] ++ replicate n ' '
      | otherwise  = replicate (i - n) ' ' ++ [toEnum (fromEnum 'A' + (n - (i - n)))] ++ replicate ((n - (i - n)) * 2 - 1) ' ' ++ [toEnum (fromEnum 'A' + (n - (i - n)))] ++ replicate (i - n) ' '

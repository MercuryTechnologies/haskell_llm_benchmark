{-# LANGUAGE BangPatterns #-}
module Trinary (readTri, showTri) where

import Prelude hiding (Maybe(..), maybe)

-- Minimal Maybe definition to avoid importing Data.Maybe
data Maybe a = Nothing | Just a

-- Equivalent of Prelude.maybe function
maybe :: b -> (a -> b) -> Maybe a -> b
maybe defaultVal _ Nothing = defaultVal
maybe _ f (Just x) = f x

-- Helper function to convert a single character to its trinary integer value (as Int).
-- Returns Nothing if the character is not a valid trinary digit.
trinaryCharToInt :: Char -> Maybe Int
trinaryCharToInt c
  | c == '0' = Just 0
  | c == '1' = Just 1
  | c == '2' = Just 2
  | otherwise = Nothing

-- | Converts a string representation of a trinary number to its decimal integer equivalent.
-- | Returns 0 if the input string is empty or contains invalid characters.
readTri :: String -> Integer
readTri s = maybe 0 id (foldl step (Just 0) s)
  where
    -- step :: Maybe Integer -> Char -> Maybe Integer
    step !maybeAcc c =
      case maybeAcc of
        Nothing -> Nothing -- Propagate failure
        Just acc ->
          case trinaryCharToInt c of
            Nothing -> Nothing -- Invalid character found
            Just digit -> Just $! (acc * 3 + fromIntegral digit) -- Strict update, convert digit to Integer

-- Helper function to convert an Integer digit (0, 1, or 2) to Char
intToTrinaryChar :: Integer -> Char
intToTrinaryChar 0 = '0'
intToTrinaryChar 1 = '1'
intToTrinaryChar 2 = '2'
intToTrinaryChar _ = error "Invalid trinary digit" -- Should not happen in showTri'

-- Recursive helper for showTri
showTri' :: Integer -> String
showTri' 0 = ""
showTri' n = showTri' (n `div` 3) ++ [intToTrinaryChar (n `mod` 3)]

-- | Converts a non-negative Integer to its trinary string representation.
showTri :: Integer -> String
showTri 0 = "0"
showTri n
  | n < 0 = error "showTri does not support negative numbers"
  | otherwise = showTri' n


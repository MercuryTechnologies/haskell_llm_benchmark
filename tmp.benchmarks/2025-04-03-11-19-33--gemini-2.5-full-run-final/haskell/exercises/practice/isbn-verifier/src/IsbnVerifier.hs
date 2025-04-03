module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)
import Data.List (splitAt)
import Data.Traversable (traverse)

-- Helper to convert a standard digit character to an Int. Returns Nothing for non-digits.
digitToIntMay :: Char -> Maybe Int
digitToIntMay c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

-- Helper to convert the last character (which can be a digit or 'X').
lastCharToIntMay :: Char -> Maybe Int
lastCharToIntMay c
  | isDigit c = Just (digitToInt c)
  | c == 'X'  = Just 10
  | otherwise = Nothing

-- Process the string: clean, validate length and characters, convert to numbers.
-- Returns Maybe [Int] representing the 10 numerical values if valid, otherwise Nothing.
processString :: String -> Maybe [Int]
processString s =
  let cleaned = filter (/= '-') s
  in if length cleaned /= 10
     then Nothing -- Incorrect length after cleaning
     else
       let (first9, lastCharStr) = splitAt 9 cleaned
           -- This head call is safe because we've already checked length is 10
           lastChar = head lastCharStr
       in -- Use traverse to apply digitToIntMay to each char in first9.
          -- If any fail, traverse returns Nothing.
          -- Combine results using Applicative style (<*>) within Maybe context.
          case (traverse digitToIntMay first9, lastCharToIntMay lastChar) of
            (Just first9Ints, Just lastInt) -> Just (first9Ints ++ [lastInt])
            _                               -> Nothing -- Invalid characters found

-- Calculate the checksum using the ISBN formula and verify if (sum `mod` 11 == 0).
calculateCheck :: [Int] -> Bool
calculateCheck digits =
  let checksum = sum $ zipWith (*) digits [10, 9 .. 1]
  in checksum `mod` 11 == 0

-- Main isbn function: ties together processing and calculation/verification.
isbn :: String -> Bool
isbn s =
  case processString s of
    Just digits -> calculateCheck digits -- If processing succeeded, calculate and check
    Nothing     -> False                 -- If processing failed (invalid format), return False

module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn s =
    case parseIsbn s of
        Just digits -> validateIsbn digits
        Nothing     -> False

-- Remove dashes, check length and valid characters, parse digits
parseIsbn :: String -> Maybe [Int]
parseIsbn str =
    let cleaned = filter (/= '-') str
    in if length cleaned /= 10
        then Nothing
        else sequence $ map charToDigit (zip [1..10] cleaned)

-- For positions 1-9: must be digit
-- For position 10: digit or 'X'
charToDigit :: (Int, Char) -> Maybe Int
charToDigit (pos, c)
    | pos < 10 && isDigit c = Just (digitToInt c)
    | pos < 10              = Nothing
    | c == 'X'              = Just 10
    | isDigit c             = Just (digitToInt c)
    | otherwise             = Nothing

validateIsbn :: [Int] -> Bool
validateIsbn digits =
    let weights = [10,9..1]
        s = sum $ zipWith (*) digits weights
    in s `mod` 11 == 0

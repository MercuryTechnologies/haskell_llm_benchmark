module Transpose (transpose) where

import Data.List (findIndices)
import Data.Maybe (isJust, fromMaybe)
-- Prelude.maximum is imported by default

transpose :: [String] -> [String]
transpose lines =
    -- If the input list of lines is empty, the transpose is also empty.
    if null lines
        then []
        else buildCols 0 maxLen lines
  where
    -- Calculate the maximum length of all lines. This determines the number of
    -- columns to process (which corresponds to the number of rows in the output).
    -- Handle the case of an empty list of lines safely.
    lineLengths = map length lines
    maxLen = if null lineLengths then 0 else maximum lineLengths

    -- buildCols generates the output rows (transposed columns) recursively.
    -- It iterates through column indices from `col` up to `maxCol - 1`.
    buildCols :: Int -> Int -> [String] -> [String]
    buildCols col maxCol inputLines
        -- Base case: If the current column index reaches the max length, we're done.
        | col >= maxCol = []
        -- Recursive step: Build the row for the current column `col`,
        -- and prepend it to the result of processing the remaining columns.
        | otherwise = buildCol col inputLines : buildCols (col + 1) maxCol inputLines

    -- buildCol generates a single output row for a given input column index `col`.
    buildCol :: Int -> [String] -> String
    buildCol col inputLines =
        let -- For the current column `col`, get `Just Char` if the char exists
            -- at `inputLines !! row !! col`, or `Nothing` otherwise.
            maybeChars :: [Maybe Char]
            maybeChars = map (safeGetChar col) inputLines

            -- Find the indices of the input lines that actually have a character
            -- at this column index (i.e., where maybeChars has a `Just` value).
            justIndices :: [Int]
            justIndices = findIndices isJust maybeChars

            -- Determine the required length of the output row. It should extend up
            -- to the last input line that had a character at this column.
            -- If `justIndices` is empty (no line had a char at this col), length is 0.
            -- Otherwise, it's `maximum justIndices + 1`.
            effectiveLen :: Int
            effectiveLen = if null justIndices then 0 else maximum justIndices + 1

            -- Take only the relevant part of `maybeChars` up to the effective length.
            relevantChars :: [Maybe Char]
            relevantChars = take effectiveLen maybeChars

            -- Convert the relevant `Maybe Char` list into the final string row.
            -- `Just c` becomes `c`, `Nothing` becomes a padding space ' '.
            paddedCol :: String
            paddedCol = map (fromMaybe ' ') relevantChars
        in paddedCol

    -- Safely attempts to get the character at index `idx` from string `s`.
    -- Returns `Just char` if `idx` is within bounds, `Nothing` otherwise.
    safeGetChar :: Int -> String -> Maybe Char
    safeGetChar idx s
        | idx < length s = Just (s !! idx)
        | otherwise      = Nothing

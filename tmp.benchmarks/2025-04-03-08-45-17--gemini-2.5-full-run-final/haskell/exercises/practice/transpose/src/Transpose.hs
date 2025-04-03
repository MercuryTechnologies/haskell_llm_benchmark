module Transpose (transpose) where

import Data.List (findIndices, maximum, unfoldr)
import Data.Maybe (fromMaybe, isJust)

-- Helper to safely get a character from a string at a given index.
-- Returns Nothing if the index is out of bounds.
safeGet :: Int -> String -> Maybe Char
safeGet i s
  | i >= 0 && i < length s = Just (s !! i)
  | otherwise              = Nothing

-- | Transposes a list of strings according to the specified rules.
--
--   Example 1:
--   transpose ["ABC", "DEF"] == ["AD", "BE", "CF"]
--
--   Example 2 (Padding):
--   transpose ["ABC", "DE"] == ["AD", "BE", "C"]
--
--   Example 3 (Padding):
--   transpose ["AB", "DEF"] == ["AD", "BE", " F"]
transpose :: [String] -> [String]
transpose lines = unfoldr buildRow 0
  where
    -- buildRow attempts to build the next row of the transposed output.
    -- It takes the current column index 'colIdx' of the *input* matrix.
    -- This column index corresponds to the row index of the *output* matrix.
    --
    -- It returns Nothing if there are no more characters in this column
    -- (meaning all input lines are shorter than this index).
    --
    -- Otherwise, it returns Just (transposedRowString, nextColIdx), where
    -- transposedRowString is the calculated row for the output, and
    -- nextColIdx is the index for the next iteration (colIdx + 1).
    buildRow :: Int -> Maybe (String, Int)
    buildRow colIdx =
      -- For the current column index, get the character (or Nothing if index is out of bounds)
      -- from each input line. This gives a list representing the column conceptually.
      let maybeChars :: [Maybe Char]
          maybeChars = map (safeGet colIdx) lines

          -- Find the indices (0-based) within maybeChars where we found a character (Just Char).
          -- These correspond to the input lines that have a character at the current colIdx.
          justIndices :: [Int]
          justIndices = findIndices isJust maybeChars

      -- If justIndices is empty, it means no input line has a character at this
      -- column index (or beyond). We have processed all columns containing characters.
      in if null justIndices
           then Nothing -- Stop the unfoldr process.
           else
             -- If there are characters in this column, we need to build the output row.
             -- Find the index of the *last* input line that contributed a character
             -- to this column. This determines the required length of the output row.
             let lastJustIndex :: Int
                 lastJustIndex = maximum justIndices

                 -- Take the portion of maybeChars up to and including the last Just Char.
                 -- This effectively trims trailing Nothings that don't need padding.
                 relevantMaybes :: [Maybe Char]
                 relevantMaybes = take (lastJustIndex + 1) maybeChars

                 -- Build the actual output row string.
                 -- Map over the relevantMaybes:
                 -- - If it's (Just c), use the character c.
                 -- - If it's Nothing (meaning an earlier input line was shorter than a later one
                 --   at this column index), replace it with a space (' ') according to the padding rule.
                 transposedRow :: String
                 transposedRow = map (fromMaybe ' ') relevantMaybes

             -- Return the constructed row and the index for the next column.
             in Just (transposedRow, colIdx + 1)

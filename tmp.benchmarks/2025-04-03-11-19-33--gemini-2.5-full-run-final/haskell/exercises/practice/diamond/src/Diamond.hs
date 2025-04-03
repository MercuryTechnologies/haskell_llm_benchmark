module Diamond (diamond) where

import Data.Char (isUpper, ord)

-- | Generates a diamond pattern for a given uppercase letter.
-- Returns Nothing if the input character is not an uppercase letter.
diamond :: Char -> Maybe [String]
diamond c
  | not (isUpper c) = Nothing -- Input must be an uppercase letter
  | otherwise       = Just $ generateDiamondRows c

-- | Generates the list of strings representing the diamond rows.
generateDiamondRows :: Char -> [String]
generateDiamondRows maxChar =
  let -- Calculate the zero-based index of the max character ('A' -> 0, 'B' -> 1, ...)
      n = ord maxChar - ord 'A'
      -- Generate the characters for the top half (including middle) and bottom half
      chars = ['A'..maxChar] ++ reverse ['A'..pred maxChar]
  -- Map each character to its corresponding row string
  in map (generateRow n) chars

-- | Generates a single row string for a given character within the diamond context.
-- Takes n (the max index) and the character for the current row.
generateRow :: Int -> Char -> String
generateRow n char =
  let -- Calculate the zero-based index of the current character
      k = ord char - ord 'A'
      -- Calculate the number of outer spaces on each side
      outerSpaceCount = n - k
      -- Create the outer padding string
      outerPadding = replicate outerSpaceCount ' '
  in if char == 'A'
       -- Special case for 'A': just outer padding + 'A' + outer padding
       then outerPadding ++ "A" ++ outerPadding
       else -- For other characters: outer padding + char + inner padding + char + outer padding
            -- Calculate the number of inner spaces between the two characters
            let innerSpaceCount = 2 * k - 1
                -- Create the inner padding string
                innerPadding = replicate innerSpaceCount ' '
            in outerPadding ++ [char] ++ innerPadding ++ [char] ++ outerPadding

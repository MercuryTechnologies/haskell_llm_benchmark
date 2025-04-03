module Say (inEnglish) where

import Data.List (intercalate)
import Data.Maybe (catMaybes) -- Used to filter out Nothing values cleanly

-- | Converts a number into its English representation.
-- | Returns Nothing if the number is out of the range [0, 999_999_999_999].
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n >= 1000000000000 = Nothing -- Range check: 0 to 999,999,999,999
  | n == 0                      = Just "zero"
  | otherwise                   = Just $ processChunks n

-- | Processes the number by breaking it into chunks of 1000.
processChunks :: Integer -> String
processChunks n =
  let chunks = chunkNum n -- Get chunks [units, thousands, millions, ...] e.g., 12345 -> [345, 12]
      numChunks = length chunks
      -- Generate Maybe String parts for each chunk, pairing with scale index
      -- zip chunks [0..] pairs each chunk with its scale index (0 for units, 1 for thousands, etc.)
      maybeParts = [ sayChunk chunk scaleIdx | (chunk, scaleIdx) <- zip chunks [0..numChunks - 1] ]
      -- Filter out Nothings (which represent zero chunks that shouldn't be spoken)
      validParts = catMaybes maybeParts
  -- Reverse the parts to get the correct order (e.g., "million" before "thousand") and join with spaces
  in intercalate " " $ reverse validParts

-- | Converts a single chunk (a number between 0 and 999) to words and adds the appropriate scale word.
-- | Returns Nothing if the chunk value is 0, as zero chunks are typically not spoken (e.g., "one million" not "one million zero thousand zero").
sayChunk :: Integer -> Int -> Maybe String
sayChunk chunk scaleIdx
  | chunk == 0 = Nothing -- Ignore zero chunks
  | otherwise  =
      let saidChunk = sayUnder1000 chunk -- Convert the 0-999 chunk to words (e.g., 345 -> "three hundred forty-five")
          scale = scaleWords !! scaleIdx -- Get the scale word (e.g., "thousand", "million") based on index
      -- Add the scale word if it's not empty (scale word for index 0 is "")
      in Just $ if null scale then saidChunk else saidChunk ++ " " ++ scale

-- | Converts a number n (0 <= n < 1000) into its English representation.
sayUnder1000 :: Integer -> String
sayUnder1000 n
  | n < 0 || n >= 1000 = error "sayUnder1000 called with out-of-range number" -- Safety check
  | n < 20   = units !! fromIntegral n -- Numbers 0-19 are unique
  | n < 100  = combineTens (n `div` 10) (n `mod` 10) -- Numbers 20-99 follow a pattern
  | otherwise = combineHundreds (n `div` 100) (n `mod` 100) -- Numbers 100-999

-- | Helper for numbers under 1000: combines the hundreds part and the remainder (0-99).
-- | Example: combineHundreds 3 45 -> "three hundred forty-five"
-- | Example: combineHundreds 3 0 -> "three hundred"
combineHundreds :: Integer -> Integer -> String
combineHundreds h r -- h: hundreds digit (1-9), r: remainder (0-99)
  | r == 0    = units !! fromIntegral h ++ " hundred" -- e.g., "three hundred"
  | otherwise = units !! fromIntegral h ++ " hundred " ++ sayUnder1000 r -- e.g., "three hundred forty-five"

-- | Helper for numbers under 100 (specifically 20-99): combines tens and units.
-- | Example: combineTens 2 0 -> "twenty"
-- | Example: combineTens 2 1 -> "twenty-one"
combineTens :: Integer -> Integer -> String
combineTens t u -- t: tens digit (2-9), u: units digit (0-9)
  | u == 0    = tens !! fromIntegral t -- e.g., "twenty", "thirty"
  | otherwise = tens !! fromIntegral t ++ "-" ++ units !! fromIntegral u -- e.g., "twenty-one"

-- | Splits a non-negative number into chunks of 1000.
-- | The chunks are ordered from least significant (units place) to most significant.
-- | Example: chunkNum 1234567 -> [567, 234, 1]
chunkNum :: Integer -> [Integer]
chunkNum n
 | n < 0 = error "chunkNum called with negative number" -- Should be handled by inEnglish
 | n == 0 = [0] -- Necessary for the recursive definition, though inEnglish handles n=0 input.
 | otherwise = chunk' n
  where
    chunk' 0 = [] -- Base case for recursion
    chunk' x = (x `mod` 1000) : chunk' (x `div` 1000) -- Extract the last 3 digits and recurse

-- Word lists for numbers and scales

-- | English words for numbers 0 through 19.
units :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
         "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

-- | English words for tens (20, 30,... 90). Index 0 and 1 are unused.
tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- | Scale words corresponding to chunks (units, thousands, millions, billions, trillions).
scaleWords :: [String]
scaleWords = ["", "thousand", "million", "billion", "trillion"] -- Index corresponds to chunk position (0 = units, 1 = thousands, etc.)

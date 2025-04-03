module Say (inEnglish) where

-- Maps for basic numbers
sayUnitsMap :: [(Integer, String)]
sayUnitsMap = zip [0..19] [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
                          , "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" ]

sayTensMap :: [(Integer, String)]
sayTensMap = zip [2..9] ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- Helper function to look up number words, returning "" for unknown (shouldn't happen with valid input)
lookupWord :: Integer -> [(Integer, String)] -> String
lookupWord n table = maybe "" id (lookup n table)

-- Converts numbers 0-19 to English
sayUnits :: Integer -> String
sayUnits n = lookupWord n sayUnitsMap

-- Converts multiples of ten (20, 30...90) to English
sayTens :: Integer -> String
sayTens n = lookupWord n sayTensMap

-- Converts numbers 0-99 to English
sayBelow100 :: Integer -> String
sayBelow100 n
  | n < 0   = error "sayBelow100: negative input"
  | n < 20  = sayUnits n
  | n < 100 =
      let (t, u) = n `divMod` 10
          tenPart = sayTens t
          unitPart = if u == 0 then "" else "-" ++ sayUnits u
      in tenPart ++ unitPart
  | otherwise = error "sayBelow100: input >= 100"

-- Converts numbers 0-999 to English
sayBelow1000 :: Integer -> String
sayBelow1000 n
  | n < 0    = error "sayBelow1000: negative input"
  | n < 100  = sayBelow100 n
  | n < 1000 =
      let (h, rest) = n `divMod` 100
          hundredPart = sayUnits h ++ " hundred"
          restPart = if rest == 0 then "" else " " ++ sayBelow100 rest
      in hundredPart ++ restPart
  | otherwise = error "sayBelow1000: input >= 1000"

-- Scale words
scales :: [String]
scales = ["", "thousand", "million", "billion"]

-- Main function
inEnglish :: Integer -> Maybe String
inEnglish n
  -- Check if the number is within the valid range (0 to 999,999,999,999)
  | n < 0 || n >= 1000000000000 = Nothing
  -- Handle the edge case for zero
  | n == 0 = Just "zero"
  -- Process valid, non-zero numbers
  | otherwise = Just $ unwords $ process n 0
  where
    -- Recursive helper function to process the number in chunks of 1000
    -- Takes the remaining number and the current scale index (0 for units, 1 for thousands, etc.)
    -- Returns a list of strings representing the parts of the number in English (e.g., ["one million", "two hundred thirty-four thousand"])
    process :: Integer -> Int -> [String]
    process 0 _ = [] -- Base case: number is fully processed
    process num scaleIdx
      -- Safety check against exceeding defined scales (shouldn't happen with initial range check)
      | scaleIdx >= length scales = error "Number too large for defined scales"
      | otherwise =
          let chunk = num `mod` 1000 -- The current chunk of three digits
              rest  = num `div` 1000 -- The rest of the number
              
              -- Recursively process the higher-order chunks first
              processedRest = process rest (scaleIdx + 1)
              
              -- Process the current chunk if it's not zero
              currentPart = if chunk == 0
                            then [] -- Don't add anything for a zero chunk
                            else [sayBelow1000 chunk ++ maybeScale] -- Convert chunk to words and add scale
                            
              -- Determine the scale word ("thousand", "million", etc.)
              -- No scale word for the first chunk (index 0) or if the chunk is zero
              maybeScale = if scaleIdx == 0 then "" else " " ++ (scales !! scaleIdx)
              
          -- Combine the parts: higher-order parts first, then the current part
          in processedRest ++ currentPart

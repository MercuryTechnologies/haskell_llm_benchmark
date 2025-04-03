module Say (inEnglish) where

-- | Convert an Integer into its English representation, for numbers 0 to 999,999,999,999.
-- | Returns Nothing if the number is out of bounds.
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (concatWords (convertNumber n))

-- | Break the number into words by processing it in chunks of up to three digits.
convertNumber :: Integer -> [String]
convertNumber n =
  let groups = chunk n
      scales = ["", "thousand", "million", "billion"]
      groupsWithScales = zip groups scales
      -- We need to reverse the order as the first chunk is the least significant.
      wordsList = reverse [ if grp /= 0 
                              then convertLessThan1000 grp ++ (if scale /= "" then " " ++ scale else "")
                              else ""
                           | (grp, scale) <- groupsWithScales ]
  in filter (not . null) wordsList

-- | Split a number into a list of three-digit chunks; least significant chunk first.
chunk :: Integer -> [Integer]
chunk 0 = []
chunk n = let (q, r) = n `divMod` 1000 in r : chunk q

-- | Convert a number less than 1000 into words.
convertLessThan1000 :: Integer -> String
convertLessThan1000 n
  | n < 100   = convertLessThan100 n
  | otherwise =
      let hundreds = n `div` 100
          remainder = n `mod` 100
          hundredsStr = ones !! fromIntegral hundreds ++ " hundred"
          remainderStr = if remainder /= 0 then " " ++ convertLessThan100 remainder else ""
      in hundredsStr ++ remainderStr

-- | Convert a number less than 100 into words.
convertLessThan100 :: Integer -> String
convertLessThan100 n
  | n < 20 = ones !! fromIntegral n
  | otherwise =
      let tensPart = tens !! fromIntegral (n `div` 10)
          onesDigit = n `mod` 10
          onesStr = if onesDigit /= 0 then "-" ++ ones !! fromIntegral onesDigit else ""
      in tensPart ++ onesStr

ones :: [String]
ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

-- | Join a list of words into a single string separated by a space.
concatWords :: [String] -> String
concatWords = unwords

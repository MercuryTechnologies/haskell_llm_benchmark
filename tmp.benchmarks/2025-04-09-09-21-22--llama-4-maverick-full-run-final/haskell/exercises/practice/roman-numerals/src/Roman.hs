module Roman (numerals) where

-- Define a list of tuples containing Roman numerals and their corresponding Arabic values
romanNumerals :: [(String, Integer)]
romanNumerals = [ ("M", 1000), ("CM", 900), ("D", 500), ("CD", 400)
                , ("C", 100), ("XC", 90), ("L", 50), ("XL", 40)
                , ("X", 10), ("IX", 9), ("V", 5), ("IV", 4), ("I", 1)
                ]

-- Implement the numerals function
numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3999 = Nothing
  | otherwise         = Just (toRoman n)

-- Helper function to convert a number to Roman numeral
toRoman :: Integer -> String
toRoman n = concatMap (uncurry replicateRoman) (applyRomanNumerals n romanNumerals)
  where
    replicateRoman :: String -> Integer -> String
    replicateRoman roman count = concat (replicate (fromIntegral count) roman)

    applyRomanNumerals :: Integer -> [(String, Integer)] -> [(String, Integer)]
    applyRomanNumerals _ [] = []
    applyRomanNumerals num ((roman, value):rest)
      | num >= value = (roman, num `div` value) : applyRomanNumerals (num `mod` value) rest
      | otherwise    = applyRomanNumerals num rest

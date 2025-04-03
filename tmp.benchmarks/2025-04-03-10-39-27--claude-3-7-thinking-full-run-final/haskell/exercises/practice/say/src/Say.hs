module Say (inEnglish) where

-- Convert a number to English words
inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing  -- Out of range
  | otherwise = Just $ spellOut n

-- Function to spell out a number in English
spellOut :: Integer -> String
spellOut 0 = "zero"                      -- Special case for zero
spellOut n = 
  let chunks = chunksOf3 n
      scaledChunks = zipWith chunkToWords chunks (reverse $ take (length chunks) scales)
  in unwords $ filter (not . null) scaledChunks

-- Convert chunk to words with scale
chunkToWords :: Integer -> String -> String
chunkToWords 0 _ = ""                    -- Skip zero chunks
chunkToWords chunk scale = 
  let chunkWords = spellChunk chunk
  in if null scale then chunkWords else chunkWords ++ " " ++ scale

-- Spell out a single chunk (0-999)
spellChunk :: Integer -> String
spellChunk n
  | n < 100 = spellUnder100 n
  | n `mod` 100 == 0 = spellUnder100 (n `div` 100) ++ " hundred"
  | otherwise = spellUnder100 (n `div` 100) ++ " hundred " ++ spellUnder100 (n `mod` 100)

-- Convert numbers under 100 to words
spellUnder100 :: Integer -> String
spellUnder100 n
  | n < 20 = singles !! fromIntegral n
  | n `mod` 10 == 0 = tensNames !! (fromIntegral (n `div` 10) - 2)
  | otherwise = tensNames !! (fromIntegral (n `div` 10) - 2) ++ "-" ++ singles !! fromIntegral (n `mod` 10)

-- Break a number into chunks of 3 digits
chunksOf3 :: Integer -> [Integer]
chunksOf3 0 = [0]                        -- Special case for 0
chunksOf3 n = reverse $ go n
  where
    go 0 = []
    go m =
      let (q, r) = m `divMod` 1000
      in r : go q

-- Lists of word representations
singles :: [String]
singles = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
           "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
           "seventeen", "eighteen", "nineteen"]

tensNames :: [String]
tensNames = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scales :: [String]
scales = ["", "thousand", "million", "billion", "trillion"]

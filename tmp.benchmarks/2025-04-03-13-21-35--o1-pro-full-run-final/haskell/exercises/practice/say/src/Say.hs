module Say (inEnglish) where

-- We'll produce an English phrase for numbers in the range [0..999999999999].
-- If the number is out of range, we'll return Nothing.

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0                    = Just "zero"
  | otherwise                 = Just (unwordsCompact (reverse (buildChunks (chunkThousands n))))

-- Break the integer into groups of up to three digits, from right to left.
chunkThousands :: Integer -> [Integer]
chunkThousands 0 = []
chunkThousands x =
  let (q, r) = x `divMod` 1000
  in r : chunkThousands q

-- Build up the final English words from each three-digit chunk plus a scale word.
-- We'll reverse the chunk list so the highest scale is first for convenience,
-- and then combine them in the correct order.
buildChunks :: [Integer] -> [String]
buildChunks [] = []
buildChunks xs =
  zipWith renderScale xs [0..]

-- Renders a single chunk plus the scale word if non-zero.
renderScale :: Integer -> Int -> String
renderScale 0 _       = ""
renderScale chunk idx =
  let base = renderThreeDigits chunk
      scale = scaleWord idx
  in unwordsCompact [base, scale]

-- The scale words for each group of thousands.
-- 0 -> ""
-- 1 -> "thousand"
-- 2 -> "million"
-- 3 -> "billion"
-- We only need up to 3 for up to 999,999,999,999.
scaleWord :: Int -> String
scaleWord 0 = ""
scaleWord 1 = "thousand"
scaleWord 2 = "million"
scaleWord 3 = "billion"
scaleWord _ = ""  -- we won't actually reach beyond billion in our chosen range

-- Render a number 0..999 in English (without scale words).
renderThreeDigits :: Integer -> String
renderThreeDigits n
  | n < 100   = renderTwoDigits (fromIntegral n)
  | otherwise =
      let hundreds = n `div` 100
          remainder = n `mod` 100
          hundredsPart = onesList !! fromIntegral hundreds ++ " hundred"
      in if remainder == 0
         then hundredsPart
         else hundredsPart ++ " " ++ renderTwoDigits (fromIntegral remainder)

-- Render a number 0..99 in English, hyphenating if needed (e.g. "twenty-two").
renderTwoDigits :: Int -> String
renderTwoDigits x
  | x < 20    = onesList !! x
  | tens == 0 = onesList !! x  -- covers x<20 case, though we already handle that above
  | ones == 0 = tensList !! tens
  | otherwise = tensList !! tens ++ "-" ++ onesList !! ones
  where
    tens = x `div` 10
    ones = x `mod` 10

-- Words for 0..19
onesList :: [String]
onesList =
  [ "zero", "one", "two", "three", "four", "five"
  , "six", "seven", "eight", "nine", "ten", "eleven"
  , "twelve", "thirteen", "fourteen", "fifteen", "sixteen"
  , "seventeen", "eighteen", "nineteen"
  ]

-- Tens words from 0..9 (we only use indices 2..9 for "twenty", "thirty", etc.)
tensList :: [String]
tensList =
  [ "", "", "twenty", "thirty", "forty", "fifty"
  , "sixty", "seventy", "eighty", "ninety"
  ]

-- Filter out empty strings and combine with spaces.
unwordsCompact :: [String] -> String
unwordsCompact = unwords . filter (not . null)

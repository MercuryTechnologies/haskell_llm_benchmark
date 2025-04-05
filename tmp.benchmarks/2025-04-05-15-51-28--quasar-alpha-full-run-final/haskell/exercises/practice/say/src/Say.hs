module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ intercalate " " $ filter (not . null) $ combineChunks $ zip chunks scaleNames
  where
    chunks = reverse $ splitByThousands n
    scaleNames = ["", "thousand", "million", "billion", "trillion"]

combineChunks :: [(Integer, String)] -> [String]
combineChunks = reverse . map chunkToWords . filter ((/= 0) . fst)
  where
    chunkToWords (num, scale)
      | null wordsInChunk = ""
      | null scale = wordsInChunk
      | otherwise = wordsInChunk ++ " " ++ scale
      where
        wordsInChunk = numberToWords num

splitByThousands :: Integer -> [Integer]
splitByThousands 0 = []
splitByThousands x = splitByThousands (x `div` 1000) ++ [x `mod` 1000]

numberToWords :: Integer -> String
numberToWords n
  | n == 0 = ""
  | n < 20 = units !! fromIntegral n
  | n < 100 =
      let (tensDigit, unitDigit) = n `divMod` 10
          tensWord = tens !! fromIntegral tensDigit
          unitWord = if unitDigit /= 0 then "-" ++ units !! fromIntegral unitDigit else ""
       in tensWord ++ unitWord
  | n < 1000 =
      let (hundredsDigit, rest) = n `divMod` 100
          hundredsWord = units !! fromIntegral hundredsDigit ++ " hundred"
          restWord = if rest /= 0 then " " ++ numberToWords rest else ""
       in hundredsWord ++ restWord
  | otherwise = error "numberToWords only supports numbers < 1000"

units :: [String]
units =
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  , "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eighteen"
  , "nineteen"
  ]

tens :: [String]
tens =
  [ ""
  , ""
  , "twenty"
  , "thirty"
  , "forty"
  , "fifty"
  , "sixty"
  , "seventy"
  , "eighty"
  , "ninety"
  ]

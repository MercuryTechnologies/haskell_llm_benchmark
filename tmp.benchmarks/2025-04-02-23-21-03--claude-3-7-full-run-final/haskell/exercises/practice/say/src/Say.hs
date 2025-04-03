module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | otherwise = Just $ spell n

spell :: Integer -> String
spell 0 = "zero"
spell n = trim $ spellNonZero n

spellNonZero :: Integer -> String
spellNonZero n
  | n < 0 = error "Negative numbers not supported"
  | n == 0 = ""
  | n < 20 = units !! fromIntegral n
  | n < 100 = let (tens, ones) = n `divMod` 10
               in tens' !! fromIntegral (tens - 2) ++ 
                  (if ones == 0 then "" else "-" ++ units !! fromIntegral ones)
  | n < 1000 = let (hundreds, rest) = n `divMod` 100
                in units !! fromIntegral hundreds ++ " hundred" ++ 
                   (if rest == 0 then "" else " " ++ spellNonZero rest)
  | otherwise = let (chunk, rest) = n `divMod` 1000
                    scaleWord = getScaleWord n
                 in spellNonZero chunk ++ scaleWord ++ 
                    (if rest == 0 then "" else " " ++ spellNonZero rest)

getScaleWord :: Integer -> String
getScaleWord n
  | n < 1000 = ""
  | n < 1000000 = " thousand"
  | n < 1000000000 = " million"
  | n < 1000000000000 = " billion"
  | otherwise = " trillion"

units :: [String]
units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 
         "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", 
         "seventeen", "eighteen", "nineteen"]

tens' :: [String]
tens' = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

trim :: String -> String
trim = unwords . words

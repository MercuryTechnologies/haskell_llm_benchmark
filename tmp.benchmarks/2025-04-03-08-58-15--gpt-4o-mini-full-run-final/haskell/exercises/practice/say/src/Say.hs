module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 || n > 999999999999 = Nothing
    | n == 0 = Just "zero"
    | otherwise = Just (convert n)

convert :: Integer -> String
convert n
    | n < 100 = basic n
    | otherwise = let (thousands, remainder) = n `divMod` 1000
                      in convertThousands thousands ++ (if remainder > 0 then " " ++ basic remainder else "")

basic :: Integer -> String
basic n
    | n < 20 = units !! fromIntegral n
    | n < 100 = let (tensPlace, unitsPlace) = n `divMod` 10
                 in tens !! fromIntegral tensPlace ++ (if unitsPlace > 0 then "-" ++ units !! fromIntegral unitsPlace else "")
    | otherwise = ""

convertThousands :: Integer -> String
convertThousands n
    | n < 1000 = basic n
    | otherwise = let (millions, thousands) = n `divMod` 1000
                      in convertThousands millions ++ " thousand"

units :: [String]
units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
          "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

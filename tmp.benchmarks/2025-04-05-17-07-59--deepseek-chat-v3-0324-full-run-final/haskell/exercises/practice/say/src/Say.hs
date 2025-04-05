module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just (convert n)

convert :: Integer -> String
convert 0 = ""
convert n
  | n < 20 = ones !! fromIntegral n
  | n < 100 = tens !! fromIntegral (n `div` 10 - 2) ++ 
              if n `mod` 10 /= 0 then "-" ++ convert (n `mod` 10) else ""
  | n < 1000 = convert (n `div` 100) ++ " hundred" ++ 
               if n `mod` 100 /= 0 then " " ++ convert (n `mod` 100) else ""
  | otherwise = 
      let (scale, name) = last $ filter ((<=n) . fst) scales
          (d, m) = (n `div` scale, n `mod` scale)
      in convert d ++ " " ++ name ++ if m /= 0 then " " ++ convert m else ""

ones :: [String]
ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
        "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scales :: [(Integer, String)]
scales = [(1000000000000, "trillion"), (1000000000, "billion"), 
          (1000000, "million"), (1000, "thousand"), (1, "")]

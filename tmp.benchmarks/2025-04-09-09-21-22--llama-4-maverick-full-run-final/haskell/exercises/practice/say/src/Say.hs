module Say (inEnglish) where

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n >= 10^12 = Nothing
  | otherwise = Just (say n)

say :: Integer -> String
say n
  | n == 0 = "zero"
  | n < 20 = units !! fromIntegral n
  | n < 100 = tens !! fromIntegral (n `div` 10) ++ if n `mod` 10 == 0 then "" else "-" ++ units !! fromIntegral (n `mod` 10)
  | n < 1000 = sayHundreds n
  | n < 10^6 = sayThousands n
  | n < 10^9 = sayMillions n
  | n < 10^12 = sayBillions n

units :: [String]
units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [String]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

sayHundreds :: Integer -> String
sayHundreds n =
  let hundreds = n `div` 100
      rest = n `mod` 100
  in if hundreds == 0
     then say rest
     else units !! fromIntegral hundreds ++ " hundred" ++ if rest == 0 then "" else " and " ++ say rest

sayThousands :: Integer -> String
sayThousands n =
  let thousands = n `div` 1000
      rest = n `mod` 1000
  in say thousands ++ " thousand" ++ if rest == 0 then "" else " " ++ say rest

sayMillions :: Integer -> String
sayMillions n =
  let millions = n `div` 10^6
      rest = n `mod` 10^6
  in say millions ++ " million" ++ if rest == 0 then "" else " " ++ say rest

sayBillions :: Integer -> String
sayBillions n =
  let billions = n `div` 10^9
      rest = n `mod` 10^9
  in say billions ++ " billion" ++ if rest == 0 then "" else " " ++ say rest

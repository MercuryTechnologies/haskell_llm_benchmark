module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 || n > 999999999999 = Nothing
  | n == 0 = Just "zero"
  | otherwise = Just $ numberToEnglish n

numberToEnglish :: Integer -> String
numberToEnglish n
  | n < 20 = underTwenty n
  | n < 100 = tens n
  | n < 1000 = hundreds n
  | n < 1000000 = thousands n
  | n < 1000000000 = millions n
  | otherwise = billions n

underTwenty :: Integer -> String
underTwenty n = case n of
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  10 -> "ten"
  11 -> "eleven"
  12 -> "twelve"
  13 -> "thirteen"
  14 -> "fourteen"
  15 -> "fifteen"
  16 -> "sixteen"
  17 -> "seventeen"
  18 -> "eighteen"
  19 -> "nineteen"
  _ -> ""

tens :: Integer -> String
tens n
  | n < 30 = if n `mod` 10 == 0 then "twenty" else "twenty-" ++ underTwenty (n - 20)
  | n < 40 = if n `mod` 10 == 0 then "thirty" else "thirty-" ++ underTwenty (n - 30)
  | n < 50 = if n `mod` 10 == 0 then "forty" else "forty-" ++ underTwenty (n - 40)
  | n < 60 = if n `mod` 10 == 0 then "fifty" else "fifty-" ++ underTwenty (n - 50)
  | n < 70 = if n `mod` 10 == 0 then "sixty" else "sixty-" ++ underTwenty (n - 60)
  | n < 80 = if n `mod` 10 == 0 then "seventy" else "seventy-" ++ underTwenty (n - 70)
  | n < 90 = if n `mod` 10 == 0 then "eighty" else "eighty-" ++ underTwenty (n - 80)
  | otherwise = if n `mod` 10 == 0 then "ninety" else "ninety-" ++ underTwenty (n - 90)

hundreds :: Integer -> String
hundreds n =
  let h = n `div` 100
      remainder = n `mod` 100
   in if remainder == 0
        then underTwenty h ++ " hundred"
        else underTwenty h ++ " hundred " ++ numberToEnglish remainder

thousands :: Integer -> String
thousands n =
  let t = n `div` 1000
      remainder = n `mod` 1000
   in if remainder == 0
        then numberToEnglish t ++ " thousand"
        else numberToEnglish t ++ " thousand " ++ numberToEnglish remainder

millions :: Integer -> String
millions n =
  let m = n `div` 1000000
      remainder = n `mod` 1000000
   in if remainder == 0
        then numberToEnglish m ++ " million"
        else numberToEnglish m ++ " million " ++ numberToEnglish remainder

billions :: Integer -> String
billions n =
  let b = n `div` 1000000000
      remainder = n `mod` 1000000000
   in if remainder == 0
        then numberToEnglish b ++ " billion"
        else numberToEnglish b ++ " billion " ++ numberToEnglish remainder

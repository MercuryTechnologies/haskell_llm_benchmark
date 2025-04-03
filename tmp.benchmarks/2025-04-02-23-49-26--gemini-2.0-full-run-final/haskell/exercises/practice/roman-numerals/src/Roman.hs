module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing
  | otherwise = Just $ toRoman n
  where
    toRoman 0 = ""
    toRoman num
      | num >= 1000 = "M" ++ toRoman (num - 1000)
      | num >= 900  = "CM" ++ toRoman (num - 900)
      | num >= 500  = "D" ++ toRoman (num - 500)
      | num >= 400  = "CD" ++ toRoman (num - 400)
      | num >= 100  = "C" ++ toRoman (num - 100)
      | num >= 90   = "XC" ++ toRoman (num - 90)
      | num >= 50   = "L" ++ toRoman (num - 50)
      | num >= 40   = "XL" ++ toRoman (num - 40)
      | num >= 10   = "X" ++ toRoman (num - 10)
      | num >= 9    = "IX" ++ toRoman (num - 9)
      | num >= 5    = "V" ++ toRoman (num - 5)
      | num >= 4    = "IV" ++ toRoman (num - 4)
      | num >= 1    = "I" ++ toRoman (num - 1)
      | otherwise   = ""

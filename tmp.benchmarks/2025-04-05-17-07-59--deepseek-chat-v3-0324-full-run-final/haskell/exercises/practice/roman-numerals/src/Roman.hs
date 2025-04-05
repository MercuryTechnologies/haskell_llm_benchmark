module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
    | n <= 0 || n > 3999 = Nothing
    | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman 0 = ""
toRoman n
    | n >= 1000 = "M"  ++ toRoman (n - 1000)
    | n >= 900  = "CM" ++ toRoman (n - 900)
    | n >= 500  = "D"  ++ toRoman (n - 500)
    | n >= 400  = "CD" ++ toRoman (n - 400)
    | n >= 100  = "C"  ++ toRoman (n - 100)
    | n >= 90   = "XC" ++ toRoman (n - 90)
    | n >= 50   = "L"  ++ toRoman (n - 50)
    | n >= 40   = "XL" ++ toRoman (n - 40)
    | n >= 10   = "X"  ++ toRoman (n - 10)
    | n >= 9    = "IX" ++ toRoman (n - 9)
    | n >= 5    = "V"  ++ toRoman (n - 5)
    | n >= 4    = "IV" ++ toRoman (n - 4)
    | n >= 1    = "I"  ++ toRoman (n - 1)

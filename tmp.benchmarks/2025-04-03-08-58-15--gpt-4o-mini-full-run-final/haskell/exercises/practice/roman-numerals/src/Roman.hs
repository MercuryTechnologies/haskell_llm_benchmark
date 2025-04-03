module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
    | n <= 0 || n > 3999 = Nothing
    | otherwise = Just (convert n)

convert :: Integer -> String
convert 0 = ""
convert n
    | n >= 1000 = 'M' : convert (n - 1000)
    | n >= 900  = "CM" ++ convert (n - 900)
    | n >= 500  = 'D' : convert (n - 500)
    | n >= 400  = "CD" ++ convert (n - 400)
    | n >= 100  = 'C' : convert (n - 100)
    | n >= 90   = "XC" ++ convert (n - 90)
    | n >= 50   = 'L' : convert (n - 50)
    | n >= 40   = "XL" ++ convert (n - 40)
    | n >= 10   = 'X' : convert (n - 10)
    | n >= 9    = "IX" ++ convert (n - 9)
    | n >= 5    = 'V' : convert (n - 5)
    | n >= 4    = "IV" ++ convert (n - 4)
    | otherwise  = 'I' : convert (n - 1)

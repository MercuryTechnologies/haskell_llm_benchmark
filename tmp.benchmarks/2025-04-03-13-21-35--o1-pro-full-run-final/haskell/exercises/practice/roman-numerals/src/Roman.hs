module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3999 = Nothing
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman 0 = ""
toRoman n =
  let (value, symbol) = head $ dropWhile (\(v, _) -> v > n) symbols
  in symbol ++ toRoman (n - value)

symbols :: [(Integer, String)]
symbols =
  [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD")
  , (100, "C"), (90, "XC"), (50, "L"), (40, "XL")
  , (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")
  ]

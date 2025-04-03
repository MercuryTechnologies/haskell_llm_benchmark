module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n <= 0    = Nothing  -- Roman numerals don't represent zero or negative numbers
  | n > 3999  = Nothing  -- Traditional Roman numerals only go up to 3999
  | otherwise = Just (toRoman n)

toRoman :: Integer -> String
toRoman 0 = ""
toRoman n = 
  let pairs = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
               (100, "C"), (90, "XC"), (50, "L"), (40, "XL"),
               (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
      
      (value, symbol) = head $ filter (\(val, _) -> val <= n) pairs
  in symbol ++ toRoman (n - value)

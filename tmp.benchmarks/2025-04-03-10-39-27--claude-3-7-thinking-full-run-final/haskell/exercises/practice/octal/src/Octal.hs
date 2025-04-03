module Octal (readOct, showOct) where

-- | Convert an octal string to a decimal integer
-- | Treats invalid input as octal 0
readOct :: String -> Int
readOct "" = 0
readOct str = 
    if all isOctalDigit str
    then foldl' (\acc c -> (acc * 8) + charToInt c) 0 str
    else 0
  where
    isOctalDigit c = c >= '0' && c <= '7'
    charToInt c = fromEnum c - fromEnum '0'
    
    -- Strict fold implementation to ensure efficiency
    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' _ z [] = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- | Convert a decimal integer to an octal string
showOct :: Int -> String
showOct 0 = "0"
showOct n
    | n < 0     = '-' : showOct (abs n)
    | otherwise = reverse $ go n
  where
    go 0 = ""
    go num = 
        let (q, r) = num `divMod` 8
            digit = intToChar r
        in digit `seq` (digit : go q)
    intToChar i = toEnum (fromEnum '0' + i)

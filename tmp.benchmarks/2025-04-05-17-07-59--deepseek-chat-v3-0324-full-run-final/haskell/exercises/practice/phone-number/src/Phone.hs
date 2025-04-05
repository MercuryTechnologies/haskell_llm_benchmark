module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = case filter isDigit xs of
    digits@('1':rest) | isValid rest -> Just rest
    digits | isValid digits -> Just digits
    _ -> Nothing
  where
    isValid :: String -> Bool
    isValid ds = length ds == 10 && 
                 ds !! 0 `elem` ['2'..'9'] && 
                 ds !! 3 `elem` ['2'..'9']

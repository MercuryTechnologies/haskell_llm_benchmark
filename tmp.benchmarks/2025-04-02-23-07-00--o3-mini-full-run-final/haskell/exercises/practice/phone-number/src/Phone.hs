module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  case filter isDigit xs of
    [a,b,c,d,e,f,g,h,i,j] ->
      if a `elem` ['2'..'9'] && d `elem` ['2'..'9']
         then Just [a,b,c,d,e,f,g,h,i,j]
         else Nothing
    '1':rest@[a,b,c,d,e,f,g,h,i,j] ->
      if a `elem` ['2'..'9'] && d `elem` ['2'..'9']
         then Just rest
         else Nothing
    _ -> Nothing

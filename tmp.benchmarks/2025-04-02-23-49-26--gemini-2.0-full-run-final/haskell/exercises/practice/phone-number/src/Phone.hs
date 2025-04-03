module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
  let cleaned = filter isDigit xs
      len = length cleaned
  in case len of
       10 ->
         let areaCode = take 3 cleaned
         in if head areaCode == '0'
            then Nothing
            else Just cleaned
       11 -> if head cleaned == '1'
             then let numberWithoutCountryCode = tail cleaned
                      areaCode = take 3 numberWithoutCountryCode
                  in if head areaCode == '0'
                     then Nothing
                     else Just numberWithoutCountryCode
             else Nothing
       _  -> Nothing

module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
    let digits = filter isDigit xs
        cleanDigits = case digits of
            ('1':rest) | length rest == 10 -> rest
            _ -> digits
    in if length cleanDigits == 10
          && validAreaCode cleanDigits
          && validExchangeCode cleanDigits
       then Just cleanDigits
       else Nothing

validAreaCode :: String -> Bool
validAreaCode (a:_:_:_) = a `elem` ['2'..'9']
validAreaCode _ = False

validExchangeCode :: String -> Bool
validExchangeCode (_:_:_:e:_:_:_:_) = e `elem` ['2'..'9']
validExchangeCode _ = False

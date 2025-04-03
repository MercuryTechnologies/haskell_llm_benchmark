module Phone (number) where

import Data.Maybe (Maybe(..))
import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let cleaned = filter isDigit xs
    in if length cleaned == 11 && head cleaned == '1'
       then if cleaned !! 1 == '0' then Nothing else Just (tail cleaned)
       else if length cleaned == 10
            then if head cleaned == '0' then Nothing else Just cleaned
            else Nothing

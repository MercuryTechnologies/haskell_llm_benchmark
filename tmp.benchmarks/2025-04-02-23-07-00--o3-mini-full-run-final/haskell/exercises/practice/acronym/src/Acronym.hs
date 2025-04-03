module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs =
  let replaced = map (\c -> if c == '-' then ' ' else c) xs
      cleaned  = filter (\c -> isAlpha c || c == ' ') replaced
      wordsList = words cleaned
      acronymWord w =
        let capitals = filter isUpper w
        in if null capitals then [toUpper (head w)] else capitals
  in concatMap acronymWord wordsList

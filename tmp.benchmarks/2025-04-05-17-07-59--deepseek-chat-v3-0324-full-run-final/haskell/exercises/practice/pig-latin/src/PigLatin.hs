module PigLatin (translate) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
  | startsWithVowelSound word = word ++ "ay"
  | hasQUPattern word = handleQU word
  | hasYAfterConsonants word = handleY word
  | otherwise = handleConsonants word

startsWithVowelSound :: String -> Bool
startsWithVowelSound [] = False
startsWithVowelSound (x:_) = toLower x `elem` "aeiou" || take 2 word `elem` ["xr", "yt"]
  where word = map toLower word

hasQUPattern :: String -> Bool
hasQUPattern word = "qu" `isPrefixOfCI` dropWhile (not . (`elem` "uU")) word
  where
    isPrefixOfCI prefix str = map toLower prefix `isPrefixOf` map toLower str

hasYAfterConsonants :: String -> Bool
hasYAfterConsonants word = 
  case dropWhile (\c -> toLower c `elem` "bcdfghjklmnpqrstvwxz") word of
    (y:rest) -> toLower y == 'y'
    _ -> False

handleQU :: String -> String
handleQU word =
  let (beforeQU, afterQU) = break (\c -> toLower c == 'q') word
      (quPart, rest) = splitAt 2 afterQU
  in rest ++ beforeQU ++ quPart ++ "ay"

handleY :: String -> String
handleY word =
  let (consonants, rest) = span (\c -> toLower c `elem` "bcdfghjklmnpqrstvwxz") word
  in rest ++ consonants ++ "ay"

handleConsonants :: String -> String
handleConsonants word =
  let (consonants, rest) = span (\c -> toLower c `elem` "bcdfghjklmnpqrstvwxz") word
  in rest ++ consonants ++ "ay"

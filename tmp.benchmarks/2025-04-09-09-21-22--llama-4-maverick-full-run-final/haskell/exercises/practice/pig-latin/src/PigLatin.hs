module PigLatin (translate) where

import Data.Char (toLower)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
  | startsWithVowelOrXRorYT word = word ++ "ay"
  | "qu" `isPrefixOf` dropWhile isConsonant word = let prefix = takeWhile isConsonant word ++ "qu" in drop (length prefix) word ++ prefix ++ "ay"
  | otherwise = let (prefix, rest) = span isConsonant word in rest ++ prefix ++ "ay"

startsWithVowelOrXRorYT :: String -> Bool
startsWithVowelOrXRorYT word = isVowel (head word) || "xr" `isPrefixOf` map toLower word || "yt" `isPrefixOf` map toLower word

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isConsonant :: Char -> Bool
isConsonant = not . isVowel

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

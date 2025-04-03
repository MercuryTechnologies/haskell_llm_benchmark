module PigLatin (translate) where

import Data.List (isPrefixOf)

translate :: String -> String
translate xs = unwords $ map translateWord (words xs)

translateWord :: String -> String
translateWord word
  | startsWithVowel word || startsWithSpecialPrefix word = word ++ "ay"
  | startsWithConsonantCluster word = moveConsonantCluster word ++ "ay"
  | startsWithConsonantFollowedByQu word = moveConsonantFollowedByQu word ++ "ay"
  | startsWithConsonantFollowedByY word = moveConsonantFollowedByY word ++ "ay"
  | otherwise = word ++ "ay"

startsWithVowel :: String -> Bool
startsWithVowel (x:_) = x `elem` "aeiou"
startsWithVowel _ = False

startsWithSpecialPrefix :: String -> Bool
startsWithSpecialPrefix word = any (`isPrefixOf` word) ["xr", "yt"]

startsWithConsonantCluster :: String -> Bool
startsWithConsonantCluster (x:xs) = not (x `elem` "aeiou") && not (startsWithConsonantFollowedByQu (x:xs)) && not (startsWithConsonantFollowedByY (x:xs))
startsWithConsonantCluster _ = False

startsWithConsonantFollowedByQu :: String -> Bool
startsWithConsonantFollowedByQu (x:y:xs) = not (x `elem` "aeiou") && y == 'q' && (not (null xs) && head xs == 'u')
startsWithConsonantFollowedByQu _ = False

startsWithConsonantFollowedByY :: String -> Bool
startsWithConsonantFollowedByY (x:y:_) = not (x `elem` "aeiou") && y == 'y'
startsWithConsonantFollowedByY _ = False

moveConsonantCluster :: String -> String
moveConsonantCluster word = let (consonants, rest) = span (`notElem` "aeiou") word in rest ++ consonants

moveConsonantFollowedByQu :: String -> String
moveConsonantFollowedByQu word = let (consonants, rest) = span (`notElem` "aeiou") word in drop 1 rest ++ consonants ++ "qu"

moveConsonantFollowedByY :: String -> String
moveConsonantFollowedByY (x:y:xs) = xs ++ [x] ++ "y"
moveConsonantFollowedByY word = word

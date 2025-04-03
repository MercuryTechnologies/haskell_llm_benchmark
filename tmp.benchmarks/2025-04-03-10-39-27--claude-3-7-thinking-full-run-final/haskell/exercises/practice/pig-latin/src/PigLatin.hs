module PigLatin (translate) where

import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
  | beginsWithVowelSound word = word ++ "ay"  -- Rule 1
  | "qu" `isPrefixOf` word = drop 2 word ++ "qu" ++ "ay"  -- Rule 3 (starts with qu)
  | hasConsonantsFollowedByQu word =  -- Rule 3 (consonants + qu)
      let 
        consonants = takeWhile (not . isVowel) word
        rest = drop (length consonants) word
        afterQu = drop 2 rest
      in afterQu ++ consonants ++ "qu" ++ "ay"
  | hasConsonantsFollowedByY word =  -- Rule 4
      let (beforeY, withAndAfterY) = break (== 'y') word
      in withAndAfterY ++ beforeY ++ "ay"
  | otherwise =  -- Rule 2
      let 
        consonants = takeWhile (not . isVowel) word
        rest = drop (length consonants) word
      in rest ++ consonants ++ "ay"
  where
    beginsWithVowelSound w = beginsWithVowel w || "xr" `isPrefixOf` w || "yt" `isPrefixOf` w
    hasConsonantsFollowedByQu w = 
      let 
        consonants = takeWhile (not . isVowel) w
        rest = drop (length consonants) w
      in not (null consonants) && "qu" `isPrefixOf` rest
    hasConsonantsFollowedByY w = 
      let (beforeY, rest) = break (== 'y') w
      in not (null beforeY) && all (not . isVowel) beforeY && not (null rest)

beginsWithVowel :: String -> Bool
beginsWithVowel "" = False
beginsWithVowel (x:_) = isVowel x

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

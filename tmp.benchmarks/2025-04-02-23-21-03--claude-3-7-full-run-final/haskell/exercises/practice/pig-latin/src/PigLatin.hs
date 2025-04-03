module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord [] = []
translateWord word
  -- Rule 1: Word begins with vowel or starts with "xr" or "yt"
  | isVowel (head word) || startsWith "xr" word || startsWith "yt" word = word ++ "ay"
  
  -- Rule 3: Word starts with consonants followed by "qu"
  | startsWith "qu" word = drop 2 word ++ "quay"
  | containsPrefix "qu" word = 
      let consonants = takeWhile (not . isVowel) word
          rest = drop (length consonants) word
      in drop 2 rest ++ consonants ++ "qu" ++ "ay"
  
  -- Rule 4: Word starts with consonants followed by "y"
  | 'y' `elem` word && not (isVowel (head word)) = 
      let (consonants, rest) = break (== 'y') word
      in if null rest then word ++ "ay" else tail rest ++ consonants ++ "y" ++ "ay"
  
  -- Rule 2: Word begins with one or more consonants
  | otherwise = let (consonants, rest) = span (not . isVowel) word
                in rest ++ consonants ++ "ay"

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

startsWith :: String -> String -> Bool
startsWith prefix str = take (length prefix) str == prefix

containsPrefix :: String -> String -> Bool
containsPrefix qu word = 
  let consonants = takeWhile (not . isVowel) word
  in startsWith qu (drop (length consonants) word)

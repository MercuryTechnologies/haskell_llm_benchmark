module PigLatin (translate) where

import Data.List (isPrefixOf)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
  | startsWithVowel word = word ++ "ay"
  | startsWithSpecialVowel word = word ++ "ay"
  | otherwise = let (start, rest) = splitPrefix word
                in rest ++ start ++ "ay"

startsWithVowel :: String -> Bool
startsWithVowel (c:_) = c `elem` "aeiou"
startsWithVowel _ = False

startsWithSpecialVowel :: String -> Bool
startsWithSpecialVowel w = any (`isPrefixOf` w) ["xr", "yt"]

splitPrefix :: String -> (String, String)
splitPrefix w
  | Just (pre, rest) <- splitAtQu w = (pre, rest)
  | otherwise = splitAtConsonantCluster w

splitAtQu :: String -> Maybe (String, String)
splitAtQu w
  | "qu" `isPrefixOf` w = Just ("qu", drop 2 w)
  | otherwise =
      let (pre, rest) = span isConsonant w
      in if "qu" `isPrefixOf` rest
            then Just (pre ++ "qu", drop 2 rest)
            else Nothing

splitAtConsonantCluster :: String -> (String, String)
splitAtConsonantCluster w =
  let (pre, rest) = span isConsonant w
  in case rest of
       ('y':ys) -> (pre, 'y':ys)  -- Rule 4: consonant(s) + y
       _        -> (pre, rest)

isConsonant :: Char -> Bool
isConsonant c = c `elem` ['a'..'z'] && not (c `elem` "aeiou")


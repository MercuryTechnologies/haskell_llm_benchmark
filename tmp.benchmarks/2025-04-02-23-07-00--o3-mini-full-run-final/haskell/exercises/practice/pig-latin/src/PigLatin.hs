module PigLatin (translate) where

translate :: String -> String
translate sentence = unwords $ map translateWord (words sentence)

translateWord :: String -> String
translateWord word
  | isRule1 word = word ++ "ay"
  | otherwise    = let (prefix, rest) = splitWord word
                   in rest ++ prefix ++ "ay"

isRule1 :: String -> Bool
isRule1 [] = False
isRule1 w@(x:_)
  | isVowel x        = True
  | take 2 w == "xr" = True
  | take 2 w == "yt" = True
  | otherwise        = False

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

splitWord :: String -> (String, String)
splitWord word = go "" word
  where
    go prefix [] = (prefix, [])
    go prefix s@(x:xs)
      | not (null prefix) && x == 'y' = (prefix, s)
      | x == 'q' && not (null xs) && head xs == 'u' = go (prefix ++ "qu") (tail xs)
      | isVowel x = (prefix, s)
      | otherwise = go (prefix ++ [x]) xs

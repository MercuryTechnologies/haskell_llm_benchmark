module PigLatin (translate) where

translate :: String -> String
translate = unwords . map pigLatinWord . words
  where
    pigLatinWord w
      | startsWithVowel w     = w ++ "ay"
      | startsWith "xr" w     = w ++ "ay"
      | startsWith "yt" w     = w ++ "ay"
      | otherwise             =
          let (prefix, rest) = spanConsonants w
          in rest ++ prefix ++ "ay"

    startsWith p s = take (length p) s == p

    startsWithVowel [] = False
    startsWithVowel (x:_) = x `elem` "aeiou"

    spanConsonants :: String -> (String, String)
    spanConsonants = go []
      where
        go acc [] = (acc, [])
        go acc s@(c1:c2:rest)
          | c1 `elem` vowels        = (acc, s)
          | c1 == 'q' && c2 == 'u'  = go (acc ++ [c1, c2]) rest
          | otherwise               = go (acc ++ [c1]) (c2:rest)
        go acc [c]
          | c `elem` vowels         = (acc, [c])
          | otherwise               = (acc ++ [c], "")

        vowels = "aeiou"

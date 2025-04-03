module PigLatin (translate) where

-- Define 'vowels' in the top-level so that it's in scope for all pattern matches.
vowels :: String
vowels = "aeiou"

-- | Translate an entire string from English to Pig Latin,
-- splitting it by whitespace into words, translating each word,
-- then joining them back together.
translate :: String -> String
translate = unwords . map pigLatinWord . words

-- | Translate a single word into Pig Latin according to the rules:
--   1. If the word begins with a vowel, or 'xr' or 'yt', just append "ay".
--   2. If a word begins with one or more consonants, move them to the end and append "ay".
--   3. If a word begins with zero or more consonants followed by "qu", move them (and "qu") to the end, then append "ay".
--   4. If a word starts with one or more consonants followed by "y", move the consonants before "y" to the end, then append "ay".
--
-- Examples:
--  - "apple"  -> "appleay"  (starts with vowel)
--  - "xray"   -> "xrayay"   (starts with "xr")
--  - "yttria" -> "yttriaay" (starts with "yt")
--  - "pig"    -> "igpay"    (starts with single consonant)
--  - "chair"  -> "airchay"  (starts with multiple consonants)
--  - "thrush" -> "ushthray" (starts with multiple consonants)
--  - "quick"  -> "ickquay"  (starts with "qu", no preceding consonants)
--  - "square" -> "aresquay" (starts with one consonant followed by "qu")
--  - "my"     -> "ymay"     (starts with single consonant followed by "y")

pigLatinWord :: String -> String
pigLatinWord w
  | startsWithVowel w = w ++ "ay"
  | startsWithXR w    = w ++ "ay"
  | startsWithYT w    = w ++ "ay"
  | otherwise =
      let (prefix, rest) = extractPrefix "" w
      in rest ++ prefix ++ "ay"

-- | Returns True if the string starts with a vowel: a, e, i, o, or u
startsWithVowel :: String -> Bool
startsWithVowel (x:_) = x `elem` vowels
startsWithVowel _     = False

-- | Returns True if the string starts with "xr"
startsWithXR :: String -> Bool
startsWithXR ('x':'r':_) = True
startsWithXR _           = False

-- | Returns True if the string starts with "yt"
startsWithYT :: String -> Bool
startsWithYT ('y':'t':_) = True
startsWithYT _           = False

-- | Given a word, find the prefix of leading consonants (with special handling
-- for "qu") or the consonants before "y". Returns (prefix, rest).
extractPrefix :: String -> String -> (String, String)
extractPrefix acc []         = (acc, "")
extractPrefix acc [c]
  | null acc && c `elem` vowels = (acc, [c])
  | otherwise                   = (acc ++ [c], "")
extractPrefix acc (x:y:xs)
  -- If we see "qu", move that to prefix and keep checking
  | x == 'q' && y == 'u'              = extractPrefix (acc ++ "qu") xs
  -- If x is a vowel, we stop.
  | null acc && x `elem` vowels       = (acc, x:y:xs)
  -- If we've read some consonants, and the next letter is a vowel, break there.
  | not (null acc) && x `elem` vowels = (acc, x:y:xs)
  -- If x is "y" and we already have some prefix, break.
  | x == 'y' && not (null acc)        = (acc, x:y:xs)
  -- If x alone is followed by "y" and we have prefix, break after x
  | not (null acc) && y == 'y'        = (acc ++ [x], y:xs)
  -- If x is a vowel, always break if we have any prefix
  | not (null acc) && x `elem` vowels = (acc, x:y:xs)
  -- If x is not a vowel (consonant), we keep going
  | otherwise                         = extractPrefix (acc ++ [x]) (y:xs)

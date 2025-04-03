module PigLatin (translate) where

import Data.List (isPrefixOf)
import Data.Char (isLower) -- Assuming lowercase input based on examples

-- | Translates a full sentence from English to Pig Latin.
translate :: String -> String
translate = unwords . map translateWord . words

-- | Translates a single word from English to Pig Latin based on the rules.
translateWord :: String -> String
translateWord w
    -- Rule 1: Starts with a vowel sound (a, e, i, o, u, xr, yt)
    | startsWithVowelSound w = w ++ "ay"
    -- Rules 2, 3, 4: Starts with consonants
    | otherwise =
        let (prefix, suffix) = findBreakPoint w
        in suffix ++ prefix ++ "ay"

-- | Checks if a character is a vowel (a, e, i, o, u).
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

-- | Checks if a word starts with a vowel sound according to Rule 1.
startsWithVowelSound :: String -> Bool
startsWithVowelSound s = case s of
    (c:_) | isVowel c -> True -- Starts with a vowel letter
    _                 -> "xr" `isPrefixOf` s || "yt" `isPrefixOf` s -- Starts with "xr" or "yt"

-- | Finds the point in a consonant-starting word where the initial consonant
-- | cluster (including potential "qu") ends and the rest of the word begins.
-- | Returns a tuple: (part-to-move-to-end, rest-of-word).
findBreakPoint :: String -> (String, String)
findBreakPoint s = go 0 s
  where
    go :: Int -> String -> (String, String)
    -- Base case: end of string reached (shouldn't happen with non-empty words starting with consonant)
    go n [] = splitAt n s

    go n currentWord@(c:cs)
        -- Rule 3 check: Is the current consonant 'q' followed by 'u'?
        | c == 'q', (u:_) <- cs, u == 'u' =
            let prefix = take (n + 2) s -- Include consonants before 'qu' and 'qu' itself
                suffix = drop (n + 2) s
            in (prefix, suffix)

        -- Check for end of consonant cluster: hits a vowel or 'y' (acting as a vowel)
        | isVowel c || (c == 'y' && n > 0) =
            let prefix = take n s -- Consonants before the vowel/'y'
                suffix = drop n s
            in (prefix, suffix)

        -- Rule 2 & 4 continuation: Still in a consonant cluster (or 'y' at the start)
        | otherwise = go (n + 1) cs

module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

-- | Decodes a run-length encoded string.
decode :: String -> String
decode [] = []
decode encodedText =
    let (numStr, rest) = span isDigit encodedText
    in case (numStr, rest) of
        -- No number prefix, replicate the character once
        ("", c:cs)      -> c : decode cs
        -- Number prefix found, replicate the character n times
        (numS, c:cs)    -> let n = read numS :: Int
                           in replicate n c ++ decode cs
        -- Handles cases like "" or "123" (invalid encoding ending in numbers)
        (_, [])         -> ""

-- | Encodes a string using run-length encoding.
encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    -- | Encodes a single group of identical characters.
    encodeGroup :: String -> String
    encodeGroup groupChars =
      let len = length groupChars
          char = head groupChars -- Safe because group never returns empty lists
      in if len == 1
         then [char] -- Output just the character if the run length is 1
         else show len ++ [char] -- Output count followed by character otherwise

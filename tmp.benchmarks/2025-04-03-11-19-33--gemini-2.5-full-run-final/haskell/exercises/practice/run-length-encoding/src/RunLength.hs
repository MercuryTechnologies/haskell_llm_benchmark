module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

-- | Decodes a run-length encoded string.
decode :: String -> String
decode [] = []
decode str =
    let (numStr, rest) = span isDigit str
    in if null numStr -- If the string doesn't start with a digit, it's a single character
         then head str : decode (tail str)
         else case rest of
                [] -> error "Invalid encoded string: number without character" -- According to spec, numbers are always followed by a char
                (charToRepeat:remaining) ->
                    let count = read numStr :: Int
                    in replicate count charToRepeat ++ decode remaining

-- | Encodes a string using run-length encoding.
encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup :: String -> String
    -- encodeGroup [] = "" -- This case is unreachable with `group` on non-empty lists. `group ""` returns `[]`, handled by the main function pattern.
    encodeGroup group@(c:_) = -- Pattern match ensures group is non-empty
      let n = length group
      in if n == 1
           then [c] -- If the run length is 1, just output the character
           else show n ++ [c] -- Otherwise, output the count followed by the character

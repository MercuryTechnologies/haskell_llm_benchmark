module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode = go
  where
    go "" = ""
    go xs =
      let (digits, rest) = span isDigit xs
      in case rest of
           ""     -> ""
           (c:cs) -> replicate (if null digits then 1 else read digits) c
                     ++ go cs

encode :: String -> String
encode text =
  concatMap encodeGroup (group text)
  where
    encodeGroup grp =
      let count = length grp
          c     = head grp
      in if count == 1
         then [c]
         else show count ++ [c]

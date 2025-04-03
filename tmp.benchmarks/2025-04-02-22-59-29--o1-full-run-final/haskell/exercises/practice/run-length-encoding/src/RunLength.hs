module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup xs =
      let len = length xs
          c   = head xs
      in (if len > 1 then show len else "") ++ [c]

decode :: String -> String
decode = go ""
  where
    go _    []     = ""
    go nums (x:xs)
      | isDigit x =
          go (nums ++ [x]) xs
      | otherwise =
          let count = if null nums then 1 else read nums
          in replicate count x ++ go "" xs

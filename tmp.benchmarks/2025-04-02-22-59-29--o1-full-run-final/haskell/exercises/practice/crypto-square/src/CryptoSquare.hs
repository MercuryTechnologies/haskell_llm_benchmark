module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)

encode :: String -> String
encode input =
    let normalized = filter isAlphaNum $ map toLower input
        n = length normalized
    in if n == 0
       then ""
       else
         let c = ceiling . sqrt $ (fromIntegral n :: Double)
             r = if (c - 1) * c >= n
                 then c - 1
                 else c
             columns =
               [ [ if j * c + i < n
                   then normalized !! (j * c + i)
                   else ' '
                 | j <- [0 .. r - 1]
                 ]
               | i <- [0 .. c - 1]
               ]
         in unwords columns

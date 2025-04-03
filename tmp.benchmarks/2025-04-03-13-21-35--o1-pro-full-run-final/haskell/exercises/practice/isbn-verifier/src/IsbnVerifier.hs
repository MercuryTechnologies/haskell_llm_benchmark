module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn s =
    let cleaned = filter (/= '-') s
    in
      if length cleaned /= 10
        then False
        else
          let first9 = take 9 cleaned
              lastChar = last cleaned
          in
            if not (all isDigit first9)
              then False
              else
                let d10 =
                      if lastChar == 'X'
                        then 10
                        else if isDigit lastChar
                          then digitToInt lastChar
                          else -1
                in
                  if d10 == -1
                    then False
                    else
                      let digits = map digitToInt first9 ++ [d10]
                          multipliers = [10,9..1]
                          total = sum $ zipWith (*) digits multipliers
                      in total `mod` 11 == 0

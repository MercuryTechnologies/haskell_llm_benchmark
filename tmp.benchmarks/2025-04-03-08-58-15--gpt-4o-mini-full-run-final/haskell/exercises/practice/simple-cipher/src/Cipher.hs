module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRs, newStdGen)
import Data.Char (ord, chr, isLower)

caesarDecode :: String -> String -> String
caesarDecode key encodedText = caesarEncode (negateShift key) encodedText

caesarEncode :: String -> String -> String
caesarEncode key text = zipWith shift text (cycle key)
  where
    shift t k = chr $ ((ord t - ord 'a' + ord k - ord 'a') `mod` 26) + ord 'a'
    
    negateShift k = map (\c -> chr (ord 'a' + (25 - (ord c - ord 'a')))) k

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    gen <- newStdGen
    let randomKey = take 100 $ randomRs ('a', 'z') gen
    let encodedText = caesarEncode randomKey text
    return (randomKey, encodedText)

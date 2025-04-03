module Say (inEnglish) where

import Data.List (intercalate)

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 || n > 999999999999 = Nothing
    | n == 0 = Just "zero"
    | otherwise =
        Just . unwords . filter (not . null) $
          [ sayPart (fromIntegral (n `div` 1000000000)) "billion"
          , sayPart (fromIntegral ((n `mod` 1000000000) `div` 1000000)) "million"
          , sayPart (fromIntegral ((n `mod` 1000000) `div` 1000)) "thousand"
          , sayPart (fromIntegral (n `mod` 1000)) ""
          ]

sayPart :: Int -> String -> String
sayPart 0 _ = ""
sayPart x scale =
    let wordsForX = sayUnder1000 x
    in if null wordsForX
       then ""
       else wordsForX ++ (if null scale then "" else " " ++ scale)

sayUnder1000 :: Int -> String
sayUnder1000 n
    | n == 0 = ""
    | n < 100 = sayUnder100 n
    | n < 1000 =
        let h = n `div` 100
            r = n `mod` 100
            headPart = sayUnder100 h ++ " hundred"
            tailPart = sayUnder100 r
        in if r == 0 then headPart else headPart ++ " " ++ tailPart
    | otherwise = error "Out of range for sayUnder1000"

sayUnder100 :: Int -> String
sayUnder100 n
    | n < 0 || n >= 100 = error "Out of range for sayUnder100"
    | n < 20 = ones !! n
    | otherwise =
        let t = n `div` 10
            o = n `mod` 10
            tensWord = tens !! t
        in if o == 0 then tensWord else tensWord ++ "-" ++ ones !! o
  where
    ones =
      [ "zero","one","two","three","four","five","six","seven","eight","nine"
      , "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen"
      , "seventeen","eighteen","nineteen"
      ]
    tens =
      [ "","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety" ]

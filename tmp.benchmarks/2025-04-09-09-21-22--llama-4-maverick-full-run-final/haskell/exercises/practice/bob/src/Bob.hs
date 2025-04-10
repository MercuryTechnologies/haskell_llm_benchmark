module Bob (responseFor) where

isQuestion :: String -> Bool
isQuestion xs = not (null xs) && last xs == '?'

isYelling :: String -> Bool
isYelling xs = any (\x -> elem x ['A'..'Z']) (filter (\x -> elem x ['A'..'Z'] || elem x ['a'..'z']) xs) && all (\x -> not (elem x ['a'..'z'])) (filter (\x -> elem x ['A'..'Z'] || elem x ['a'..'z']) xs)

isSilence :: String -> Bool
isSilence xs = all (\x -> elem x [' ', '\t', '\n']) xs

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

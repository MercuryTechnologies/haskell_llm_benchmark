module Brackets (arePaired) where

import qualified Data.Map as M

arePaired :: String -> Bool
arePaired = check []
  where
    bracketMap = M.fromList [('(',')'), ('[',']'), ('{','}')]
    isOpen c = c `elem` "([{" 
    isClose c = c `elem` ")]}"

    check :: [Char] -> String -> Bool
    check _ [] = True
    check stack (x:xs)
      | isOpen x = check (x:stack) xs
      | isClose x = 
          case stack of
            [] -> False
            (s:ss) -> if (M.lookup s bracketMap) == Just x then check ss xs else False
      | otherwise = check stack xs

module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = check xs []
  where
    check [] stack = null stack
    check (x:xs) stack
      | isOpen x  = check xs (x:stack)
      | isClose x = not (null stack) && matches (head stack) x && check xs (tail stack)
      | otherwise = check xs stack

    isOpen c = c `elem` "([{"
    isClose c = c `elem` ")]}"
    matches open close = (open, close) `elem` [('(',')'), ('[',']'), ('{','}')]

module FoodChain (song) where

import Data.List (intersperse)

song :: String
song = unlines $ intersperse "" versesStr
  where
    animals :: [(String, Maybe String)]
    animals =
      [ ("fly", Nothing)
      , ("spider", Just "It wriggled and jiggled and tickled inside her.")
      , ("bird", Just "How absurd to swallow a bird!")
      , ("cat", Just "Imagine that, to swallow a cat!")
      , ("dog", Just "What a hog, to swallow a dog!")
      , ("goat", Just "Just opened her throat and swallowed a goat!")
      , ("cow", Just "I don't know how she swallowed a cow!")
      , ("horse", Just "She's dead, of course!")
      ]

    versesStr :: [String]
    versesStr = [verse i | i <- [0 .. length animals - 1]]

    verse :: Int -> String
    verse i =
      let (animal, mComment) = animals !! i
          introLine = "I know an old lady who swallowed a " ++ animal ++ "."
          commentLine = maybe "" id mComment
          endingLine = "I don't know why she swallowed the fly. Perhaps she'll die."
          chainLines n = [chainLine j | j <- [n, n-1 .. 1]]
          chainLine j =
            let (curr, _) = animals !! j
                (prev, _) = animals !! (j-1)
                special = if prev == "spider" then " that wriggled and jiggled and tickled inside her" else ""
            in "She swallowed the " ++ curr ++ " to catch the " ++ prev ++ special ++ "."
          maybeToList Nothing  = []
          maybeToList (Just s) = [s]
      in if animal == "horse"
         then unlines [introLine, commentLine]
         else unlines ( [introLine] ++ maybeToList mComment ++ chainLines i ++ [endingLine] )

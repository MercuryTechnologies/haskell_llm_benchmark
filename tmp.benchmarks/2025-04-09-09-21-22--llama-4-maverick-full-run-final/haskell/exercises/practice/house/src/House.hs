module House (rhyme) where

data Verse = Verse String String (Maybe Verse)

verses :: [Verse]
verses =
  [ Verse "the house that Jack built" "" Nothing
  , Verse "the malt" "lay in" . Just $ verses !! 0
  , Verse "the rat" "ate" . Just $ verses !! 1
  , Verse "the cat" "killed" . Just $ verses !! 2
  , Verse "the dog" "worried" . Just $ verses !! 3
  , Verse "the cow with the crumpled horn" "tossed" . Just $ verses !! 4
  , Verse "the maiden all forlorn" "milked" . Just $ verses !! 5
  , Verse "the man all tattered and torn" "kissed" . Just $ verses !! 6
  , Verse "the priest all shaven and shorn" "married" . Just $ verses !! 7
  , Verse "the rooster that crowed in the morn" "woke" . Just $ verses !! 8
  , Verse "the farmer sowing his corn" "kept" . Just $ verses !! 9
  , Verse "the horse and the hound and the horn" "belonged to" . Just $ verses !! 10
  ]

verseToString :: Int -> String
verseToString n = "This is " ++ s ++ ".\n" ++ maybe "" (embed a) obj
  where
    Verse s a obj = verses !! (n-1)
    embed a' obj' = "that " ++ a' ++ " " ++ obj' ++ "\n" ++ maybe "" (embedAction obj') (object obj')
    object (Verse _ _ obj') = obj'
    embedAction obj' (Just v') = embed (action v') (subject v')
    action (Verse _ a' _) = a'
    subject (Verse s' _ _) = s'

rhyme :: String
rhyme = unlines $ map verseToString [1..12]

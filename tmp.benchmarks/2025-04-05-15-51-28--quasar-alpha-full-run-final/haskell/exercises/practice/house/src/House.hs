module House (rhyme) where

rhyme :: String
rhyme = unlines $ map unlines verses

verses :: [[String]]
verses = [buildVerse n | n <- [1..length phrases]]

buildVerse :: Int -> [String]
buildVerse n = ("This is " ++ head (phrases !! (n - 1))) : tailLines
  where
    tailLines = concatMap tailOrEmpty (reverse (take n phrases))
    tailOrEmpty (_:xs) = xs
    tailOrEmpty [] = []

phrases :: [[String]]
phrases =
  [ ["the house that Jack built."]
  , ["the malt", "that lay in the house that Jack built."]
  , ["the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the rooster that crowed in the morn", "that woke the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the farmer sowing his corn", "that kept the rooster that crowed in the morn", "that woke the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  , ["the horse and the hound and the horn", "that belonged to the farmer sowing his corn", "that kept the rooster that crowed in the morn", "that woke the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house that Jack built."]
  ]

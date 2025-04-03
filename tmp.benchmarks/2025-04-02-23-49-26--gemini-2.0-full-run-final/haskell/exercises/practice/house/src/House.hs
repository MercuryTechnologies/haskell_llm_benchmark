module House (rhyme) where

pieces :: [(String, String)]
pieces = reverse [
    ("the horse and the hound and the horn", "that belonged to the farmer sowing his corn"),
    ("the farmer sowing his corn", "that kept the rooster that crowed in the morn"),
    ("the rooster that crowed in the morn", "that woke the priest all shaven and shorn"),
    ("the priest all shaven and shorn", "that married the man all tattered and torn"),
    ("the man all tattered and torn", "that kissed the maiden all forlorn"),
    ("the maiden all forlorn", "that milked the cow with the crumpled horn"),
    ("the cow with the crumpled horn", "that tossed the dog"),
    ("the dog", "that worried the cat"),
    ("the cat", "that killed the rat"),
    ("the rat", "that ate the malt"),
    ("the malt", "that lay in the house that Jack built")
    ]

rhyme :: String
rhyme = concat $ map verse [1..length pieces]

verse :: Int -> String
verse n = let
    firstLine = "This is " ++ subject n ++ ".\n"
    restLines = concat $ reverse $ map line (take (n - 1) pieces)
    in firstLine ++ restLines ++ "\n"

subject :: Int -> String
subject n = fst (pieces !! (n - 1))

line :: (String, String) -> String
line (subject, action) = "that " ++ action ++ "\n"

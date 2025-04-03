module House (rhyme) where

rhyme :: String
rhyme = unlines [
    line "This is the house" [],
    line "This is the malt" ["that lay in the house"],
    line "This is the rat" ["that ate the malt", "that lay in the house"],
    line "This is the cat" ["that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the dog" ["that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the cow with the crumpled horn" ["that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the maiden all forlorn" ["that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the man all tattered and torn" ["that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the priest all shaven and shorn" ["that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the rooster that crowed in the morn" ["that woke the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the farmer sowing his corn" ["that kept the rooster that crowed in the morn", "that woke the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"],
    line "This is the horse and the hound and the horn" ["that belonged to the farmer sowing his corn", "that kept the rooster that crowed in the morn", "that woke the priest all shaven and shorn", "that married the man all tattered and torn", "that kissed the maiden all forlorn", "that milked the cow with the crumpled horn", "that tossed the dog", "that worried the cat", "that killed the rat", "that ate the malt", "that lay in the house"]
    ] ++ ["that Jack built."]

line :: String -> [String] -> String
line phrase clauses = phrase ++ concatMap (" " ++) clauses

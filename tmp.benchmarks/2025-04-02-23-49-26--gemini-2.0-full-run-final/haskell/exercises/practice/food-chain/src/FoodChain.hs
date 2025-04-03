module FoodChain (song) where

data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse deriving (Eq, Show)

animalVerse :: Animal -> String
animalVerse Fly = "I know an old lady who swallowed a fly.\nI don't know why she swallowed the fly. Perhaps she'll die.\n"
animalVerse Spider = "I know an old lady who swallowed a spider.\nIt wriggled and jiggled and tickled inside her.\n"
animalVerse Bird = "I know an old lady who swallowed a bird.\nHow absurd to swallow a bird!\n"
animalVerse Cat = "I know an old lady who swallowed a cat.\nImagine that, to swallow a cat!\n"
animalVerse Dog = "I know an old lady who swallowed a dog.\nWhat a hog, to swallow a dog!\n"
animalVerse Goat = "I know an old lady who swallowed a goat.\nJust opened her throat and swallowed a goat!\n"
animalVerse Cow = "I know an old lady who swallowed a cow.\nI don't know how she swallowed a cow!\n"
animalVerse Horse = "I know an old lady who swallowed a horse.\nShe's dead, of course!\n"

catchVerse :: Animal -> Animal -> String
catchVerse Spider Fly = "She swallowed the spider to catch the fly.\n"
catchVerse Bird Spider = "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n"
catchVerse Cat Bird = "She swallowed the cat to catch the bird.\n"
catchVerse Dog Cat = "She swallowed the dog to catch the cat.\n"
catchVerse Goat Dog = "She swallowed the goat to catch the dog.\n"
catchVerse Cow Goat = "She swallowed the cow to catch the goat.\n"
catchVerse _ _ = ""

animals :: [Animal]
animals = [Fly, Spider, Bird, Cat, Dog, Goat, Cow, Horse]

song :: String
song = concat $ zipWith (\animal i -> verse animal i) animals [1..]

verse :: Animal -> Int -> String
verse Horse _ = animalVerse Horse
verse animal n = animalVerse animal ++ swallowLineup (take (n - 1) $ reverse $ take (n - 1) animals)

swallowLineup :: [Animal] -> String
swallowLineup [] = "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
swallowLineup (x:xs) = catchVerse x (head xs) ++ swallowLineup xs ++ "\n"

module FoodChain (song) where

data Animal = Fly | Spider | Bird | Cat | Dog | Goat | Cow | Horse
  deriving (Eq, Enum, Bounded)

animalName :: Animal -> String
animalName Fly    = "fly"
animalName Spider = "spider"
animalName Bird   = "bird"
animalName Cat    = "cat"
animalName Dog    = "dog"
animalName Goat   = "goat"
animalName Cow    = "cow"
animalName Horse  = "horse"

verseLine1 :: Animal -> String
verseLine1 animal = "I know an old lady who swallowed a " ++ animalName animal ++ "."

verseLine2 :: Animal -> String
verseLine2 Fly    = "I don't know why she swallowed the fly. Perhaps she'll die."
verseLine2 Spider = "It wriggled and jiggled and tickled inside her."
verseLine2 Bird   = "How absurd to swallow a bird!"
verseLine2 Cat    = "Imagine that, to swallow a cat!"
verseLine2 Dog    = "What a hog, to swallow a dog!"
verseLine2 Goat   = "Just opened her throat and swallowed a goat!"
verseLine2 Cow    = "I don't know how she swallowed a cow!"
verseLine2 Horse  = "She's dead, of course!"

swallowReason :: Animal -> String
swallowReason animal = "She swallowed the " ++ animalName animal ++ " to catch the "

verse :: Animal -> String
verse animal = unlines $ 
  [verseLine1 animal, verseLine2 animal] 
  ++ if animal == minBound 
     then ["I don't know why she swallowed the fly. Perhaps she'll die."]
     else map swallowVerse (enumFromThenTo animal (pred animal) minBound)
  ++ ["" | animal /= maxBound]

swallowVerse :: Animal -> String
swallowVerse animal = swallowReason animal ++ animalName (pred animal) ++ predicate (pred animal)

predicate :: Animal -> String
predicate Spider = " that wriggled and jiggled and tickled inside her"
predicate _      = ""

song :: String
song = unlines $ map verse [minBound .. maxBound]

module FoodChain (song) where

-- Data structure to represent each animal in the food chain
data Animal = Animal
  { name :: String
  , reaction :: String
  }

-- List of animals in order they are swallowed
animals :: [Animal]
animals =
  [ Animal "fly" ""
  , Animal "spider" "It wriggled and jiggled and tickled inside her."
  , Animal "bird" "How absurd to swallow a bird!"
  , Animal "cat" "Imagine that, to swallow a cat!"
  , Animal "dog" "What a hog, to swallow a dog!"
  , Animal "goat" "Just opened her throat and swallowed a goat!"
  , Animal "cow" "I don't know how she swallowed a cow!"
  , Animal "horse" "She's dead, of course!"
  ]

-- Generate a verse for a specific animal
verse :: Int -> String
verse n
  | n == 7 = horseVerse  -- Special case for horse
  | otherwise = 
      "I know an old lady who swallowed a " ++ name animal ++ ".\n" ++
      (if reaction animal /= "" then reaction animal ++ "\n" else "") ++
      concatMap chainLine (reverse $ take n $ tail animals) ++
      "I don't know why she swallowed the fly. Perhaps she'll die."
  where
    animal = animals !! n
    
    -- Generate a line in the food chain
    chainLine :: Animal -> String
    chainLine a
      | name a == "spider" = "She swallowed the spider to catch the fly.\n"
      | name a == "bird" = "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n"
      | otherwise = "She swallowed the " ++ name a ++ " to catch the " ++ name (animals !! (animalIndex a - 1)) ++ ".\n"
    
    -- Find the index of an animal in the list
    animalIndex :: Animal -> Int
    animalIndex a = case lookup (name a) (zip (map name animals) [0..]) of
                      Just idx -> idx
                      Nothing -> error "Animal not found"

-- Special verse for the horse
horseVerse :: String
horseVerse = "I know an old lady who swallowed a horse.\nShe's dead, of course!"

-- Generate the complete song
song :: String
song = intercalate "\n\n" $ map verse [0..7]

-- Helper function to join strings with a separator
intercalate :: String -> [String] -> String
intercalate sep [] = ""
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

module FoodChain (song) where

data Animal = Animal 
  { name :: String
  , comment :: String
  , action :: Maybe String
  }

animals :: [Animal]
animals =
  [ Animal "fly" "I don't know why she swallowed the fly. Perhaps she'll die." Nothing
  , Animal "spider" "It wriggled and jiggled and tickled inside her." (Just "spider to catch the fly")
  , Animal "bird" "How absurd to swallow a bird!" (Just "bird to catch the spider that wriggled and jiggled and tickled inside her")
  , Animal "cat" "Imagine that, to swallow a cat!" (Just "cat to catch the bird")
  , Animal "dog" "What a hog, to swallow a dog!" (Just "dog to catch the cat")
  , Animal "goat" "Just opened her throat and swallowed a goat!" (Just "goat to catch the dog")
  , Animal "cow" "I don't know how she swallowed a cow!" (Just "cow to catch the goat")
  , Animal "horse" "She's dead, of course!" Nothing
  ]

verse :: Int -> String
verse n 
  | n == length animals = firstLine (animals !! (n-1)) ++ comment (animals !! (n-1)) ++ "\n"
  | n == 1 = firstLine (animals !! 0) ++ "\n"
  | otherwise = 
      firstLine (animals !! (n-1)) ++ 
      comment (animals !! (n-1)) ++ "\n" ++ 
      chain (n-1) ++ 
      comment (head animals) ++ "\n\n"

firstLine :: Animal -> String
firstLine animal = "I know an old lady who swallowed a " ++ name animal ++ ".\n"

chain :: Int -> String
chain 0 = ""
chain i = 
  case action (animals !! i) of
    Just a -> "She swallowed the " ++ a ++ ".\n" ++ chain (i-1)
    Nothing -> ""

song :: String
song = concatMap verse [1..length animals]

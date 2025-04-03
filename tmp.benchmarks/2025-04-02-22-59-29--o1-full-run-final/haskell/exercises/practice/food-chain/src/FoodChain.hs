module FoodChain (song) where

-- Data structure to hold:
--   (animal name, optional second line, bridging phrase used when referencing this animal)
animals :: [(String, String, String)]
animals =
  [ ("fly"
    , ""
    , ""  -- Empty bridging phrase for fly, so bridging doesn't force the final line here
    )
  , ("spider"
    , "It wriggled and jiggled and tickled inside her."
    , " that wriggled and jiggled and tickled inside her"
    )
  , ("bird"
    , "How absurd to swallow a bird!"
    , ""
    )
  , ("cat"
    , "Imagine that, to swallow a cat!"
    , ""
    )
  , ("dog"
    , "What a hog, to swallow a dog!"
    , ""
    )
  , ("goat"
    , "Just opened her throat and swallowed a goat!"
    , ""
    )
  , ("cow"
    , "I don't know how she swallowed a cow!"
    , ""
    )
  , ("horse"
    , "She's dead, of course!"
    , ""
    )
  ]

song :: String
song = concatMap verse [0 .. 7]

verse :: Int -> String
verse i
  | i == horseIndex = horseVerse
  | otherwise       =
      firstLine
      ++ secondLineIfAny
      ++ bridging i
      ++ "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
  where
    horseIndex = 7
    (nameI, secondLineI, _) = animals !! i
    firstLine = "I know an old lady who swallowed a " ++ nameI ++ ".\n"
    secondLineIfAny = if null secondLineI then "" else secondLineI ++ "\n"
    horseVerse =
      "I know an old lady who swallowed a horse.\n" ++
      "She's dead, of course!\n"

-- For verse i, build references backwards down to 0.
-- bridging(0) for fly is empty, so the final "I don't know why..." line is handled in the verse itself.
bridging :: Int -> String
bridging 0 = ""  -- no bridging phrase for fly
bridging i =
  "She swallowed the " ++ nameI ++ " to catch the " ++ namePrev
  ++ bridgingPhrasePrev ++ ".\n" ++ bridging (i - 1)
  where
    (nameI, _, _) = animals !! i
    (namePrev, _, bridgingPhrasePrevRaw) = animals !! (i - 1)
    bridgingPhrasePrev =
      if null bridgingPhrasePrevRaw
         then ""
         else bridgingPhrasePrevRaw

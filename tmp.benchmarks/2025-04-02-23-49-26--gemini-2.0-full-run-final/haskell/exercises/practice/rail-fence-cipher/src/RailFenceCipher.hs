module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode rails message =
  let
    fence = buildFence rails message
    in
    concatMap reverse fence

decode :: Int -> String -> String
decode rails message =
  let
    len = length message
    fence = buildFence' rails len
    indexedFence = distributeMessage rails message fence
  in
  extractMessage rails indexedFence

buildFence :: Int -> String -> [String]
buildFence rails message =
  buildFence' rails (length message)

buildFence' :: Int -> Int -> [String]
buildFence' rails len = replicate rails ""

distributeMessage :: Int -> String -> [String] -> [String]
distributeMessage rails message fence =
  let
    indices = cycle $ concat [[x | x <- [0..(rails-1)]], [x | x <- [(rails-2), (rails-3)..1]]]
    indexedMessage = zip indices message
  in
  foldl (\acc (rail, char) -> update acc rail (char : (acc !! rail))) fence indexedMessage

extractMessage :: Int -> [String] -> String
extractMessage rails fence =
  let
    indices = cycle $ concat [[x | x <- [0..(rails-1)]], [x | x <- [(rails-2), (rails-3)..1]]]
    railChars = zip indices (concatMap reverse fence)
    sortedChars = sortBy (\(rail1, _) (rail2, _) -> compare rail1 rail2) railChars
  in
  map snd sortedChars

update :: [a] -> Int -> a -> [a]
update list index element =
  let
    (left, right) = splitAt index list
  in
  left ++ (element : right)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) =
  let
    smallerOrEqual = filter (\a -> cmp a x /= GT) xs
    larger = filter (\a -> cmp a x == GT) xs
  in
  sortBy cmp smallerOrEqual ++ [x] ++ sortBy cmp larger

module BookStore (total, Book(..)) where

import Data.List (sort, group, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map -- Use Strict Map for potentially better performance
import Control.Monad.State (State, gets, modify, evalState)

-- Define the 5 book types, deriving necessary typeclasses
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Enum, Bounded, Show) -- Added Show for potential debugging

-- Price for a group of k distinct books in cents
groupPrice :: Int -> Int
groupPrice k = case k of
  1 -> 800        -- 1 * 800 * 1.00
  2 -> 1520       -- 2 * 800 * 0.95
  3 -> 2160       -- 3 * 800 * 0.90
  4 -> 2560       -- 4 * 800 * 0.80
  5 -> 3000       -- 5 * 800 * 0.75
  _ -> 0          -- Price for groups of size 0 or > 5 is 0 (or could be an error)

-- Type alias for the memoization map (stores computed prices for count lists)
type Memo = Map.Map [Int] Int

-- Recursive function to calculate minimum price using State monad for memoization
-- Input `counts` must be sorted descending and padded to length 5.
minPrice :: [Int] -> State Memo Int
minPrice counts
  -- Base case: If all counts are zero, the price is 0.
  | all (== 0) counts = return 0
  | otherwise = do
      -- Get the current memoization map
      memo <- gets id
      -- Check if the price for this count configuration is already memoized
      case Map.lookup counts memo of
        Just price -> return price -- Return memoized result
        Nothing -> do
          -- Calculate the number of distinct book types currently in the basket
          let numDistinct = length $ filter (> 0) counts

          -- Function to calculate the price if we form a group of size k
          let calculatePath :: Int -> State Memo Int
              calculatePath k = do
                -- Form the next state by decrementing the top k counts
                let (decrementedCounts, remainingCounts) = splitAt k counts
                    nextCountsRaw = map (subtract 1) decrementedCounts ++ remainingCounts
                    -- Sort the resulting counts descendingly for the recursive call and memoization key
                    nextCountsSorted = sortOn Down nextCountsRaw
                -- Recursively calculate the minimum price for the remaining books
                recursivePrice <- minPrice nextCountsSorted
                -- Return the total price for this path (group price + recursive price)
                return $ groupPrice k + recursivePrice

          -- Calculate the potential minimum prices by trying all possible group sizes (1 to numDistinct)
          prices <- mapM calculatePath [1..numDistinct]

          -- The minimum price for the current state is the minimum over all possible first groups
          let minP = minimum prices

          -- Store the calculated minimum price in the memoization map
          modify (Map.insert counts minP)
          -- Return the calculated minimum price
          return minP

-- Counts the occurrences of each book type in the basket
bookCounts :: [Book] -> [Int]
bookCounts basket =
  let
    -- Define all possible book types
    allBooks = [First .. Fifth]
    -- Group identical books and count them
    groupedBasket = group $ sort basket
    -- Create a map from book type to its count
    countsMap = Map.fromList $ map (\g -> (head g, length g)) groupedBasket
    -- Create a list of counts, one for each book type, in order. Use 0 if a book type is not present.
    countsList = map (\book -> Map.findWithDefault 0 book countsMap) allBooks
  in countsList

-- The main function exposed by the module calculates the total minimum price for the basket
total :: [Book] -> Int
total basket
  -- Handle the edge case of an empty basket
  | null basket = 0
  | otherwise =
      let
        -- Calculate the initial counts of each book type
        initialCounts = bookCounts basket
        -- Sort counts descendingly to establish a canonical form for memoization
        sortedInitialCounts = sortOn Down initialCounts
        -- Pad the counts list with zeros to ensure it always has length 5
        -- This simplifies the logic in minPrice (e.g., splitAt)
        paddedCounts = take 5 (sortedInitialCounts ++ repeat 0)
        -- Run the memoized calculation using the State monad, starting with an empty memo map
        result = evalState (minPrice paddedCounts) Map.empty
      in result

module TwoBucket (measure) where

import Control.Monad.State
import qualified Data.Set as Set

-- | Represents the state of the two buckets
type BucketState = (Int, Int)

-- | Represents a search state with the current bucket state and number of actions taken
type SearchState = (BucketState, Int)

-- | Measure the target amount using two buckets of given capacities
-- Returns (number of actions, (final bucket with target, other bucket amount))
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2 = Nothing  -- Target is larger than any bucket
  | target <= 0 = Nothing             -- Invalid target
  | cap1 <= 0 || cap2 <= 0 = Nothing  -- Invalid capacities
  | otherwise = findSolution (cap1, cap2) target

-- | Find the solution using breadth-first search
findSolution :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
findSolution (cap1, cap2) target = evalState (bfs [(startState, 1)] Set.empty) Set.empty
  where
    startState = (cap1, 0)  -- Start with bucket one filled (as per instructions)
    
    bfs :: [SearchState] -> Set.Set BucketState -> State (Set.Set BucketState) (Maybe (Int, (Int, Int)))
    bfs [] _ = return Nothing
    bfs ((state@(b1, b2), steps):queue) visited = do
      visited' <- get
      
      -- Check if we've reached the target
      if b1 == target
        then return $ Just (steps, (1, b2))
      else if b2 == target
        then return $ Just (steps, (2, b1))
      else if Set.member state visited'
        then bfs queue visited
      else do
        -- Mark current state as visited
        put $ Set.insert state visited'
        
        -- Generate next possible states
        let nextStates = filter (not . flip Set.member visited')
                       $ map (\s -> (s, steps + 1))
                       $ nextPossibleStates (cap1, cap2) (b1, b2)
        
        bfs (queue ++ nextStates) visited'

-- | Generate all possible next states from the current state
nextPossibleStates :: (Int, Int) -> BucketState -> [BucketState]
nextPossibleStates (cap1, cap2) (b1, b2) = filter validState
  [ (0, b2)                                -- Empty bucket 1
  , (b1, 0)                                -- Empty bucket 2
  , (cap1, b2)                             -- Fill bucket 1
  , (b1, cap2)                             -- Fill bucket 2
  , (max 0 (b1 - (cap2 - b2)), min cap2 (b2 + b1))  -- Pour 1 into 2
  , (min cap1 (b1 + b2), max 0 (b2 - (cap1 - b1)))  -- Pour 2 into 1
  ]
  where
    -- Ensure we don't violate the rule: after an action, starting bucket can't be empty while other is full
    validState (0, b) = b /= cap2
    validState (_, _) = True

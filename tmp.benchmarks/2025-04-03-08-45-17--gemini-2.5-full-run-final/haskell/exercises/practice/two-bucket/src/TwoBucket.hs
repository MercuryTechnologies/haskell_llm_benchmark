{-# LANGUAGE MultiWayIf #-} -- Enable MultiWayIf extension

module TwoBucket (measure) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>))
import Data.Set (Set)
import Prelude hiding (gcd) -- Use our own or import explicitly if needed from Math.NumberTheory.GCD, but Prelude's is fine

-- Greatest Common Divisor (needed for reachability check)
gcd :: Int -> Int -> Int
gcd a 0 = abs a
gcd a b = gcd b (a `mod` b)

-- State: (level1, level2)
type State = (Int, Int)
-- Queue Element: (moves, state)
type QueueElement = (Int, State)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  -- Basic validation checks
  | target <= 0 = Nothing -- Target must be positive
  | target > max cap1 cap2 = Nothing -- Target cannot exceed the larger bucket's capacity
  | target `mod` gcd cap1 cap2 /= 0 = Nothing -- Target must be reachable (divisible by GCD of capacities)

  -- Handle cases where the target is one of the capacities directly
  -- If target is cap1, the first move (fill bucket 1) achieves it.
  | target == cap1 = Just (1, (1, 0))
  -- If target is cap2, and we start by filling bucket 1, we might reach it later.
  -- However, if cap1 can reach cap2 directly (e.g. cap1=0), BFS handles it.
  -- A special case: if cap2 is the target and cap2 is filled first (by swapping), it takes 1 move.
  -- But the problem asks us to assume starting with bucket 1.
  -- Let's consider if filling bucket 2 immediately after bucket 1 achieves the goal.
  -- State after 1 move: (cap1, 0)
  -- State after 2 moves (fill bucket 2): (cap1, cap2) -> if cap2 == target, this is a potential path.
  -- Let BFS find the shortest path naturally.

  -- Perform BFS starting from filling bucket 1
  | otherwise = bfs (cap1, cap2) target

bfs :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
bfs (cap1, cap2) target =
  let
    -- Initial state: Fill bucket 1 (first move)
    initialState = (cap1, 0)
    initialMoves = 1
    initialQueue = Seq.singleton (initialMoves, initialState)
    initialVisited = Set.singleton initialState

    -- Forbidden state when starting with bucket 1: Bucket 1 empty, Bucket 2 full
    forbiddenState = (0, cap2)

    -- Helper function for BFS loop
    go :: Seq QueueElement -> Set State -> Maybe (Int, (Int, Int))
    go queue visited =
      case Seq.viewl queue of
        Seq.EmptyL -> Nothing -- Target not found
        (moves, currentState@(l1, l2)) Seq.:< restQueue ->
          -- Check if goal reached
          if | l1 == target -> Just (moves, (1, l2)) -- Target in bucket 1
             | l2 == target -> Just (moves, (2, l1)) -- Target in bucket 2
             | otherwise ->
                 -- Generate next possible states from current state
                 let
                   nextStatesRaw =
                     [ -- Fill bucket 1
                       (cap1, l2)
                       -- Fill bucket 2
                     , (l1, cap2)
                       -- Empty bucket 1
                     , (0, l2)
                       -- Empty bucket 2
                     , (l1, 0)
                       -- Pour 1 to 2
                     , let pourAmount12 = min l1 (cap2 - l2)
                       in (l1 - pourAmount12, l2 + pourAmount12)
                       -- Pour 2 to 1
                     , let pourAmount21 = min l2 (cap1 - l1)
                       in (l1 + pourAmount21, l2 - pourAmount21)
                     ]
                   -- Filter out the forbidden state and states already visited
                   validNewStates = filter (\s -> s /= forbiddenState && not (Set.member s visited)) nextStatesRaw
                   -- Update visited set and queue for the next iteration
                   newVisited = foldr Set.insert visited validNewStates
                   newQueue = foldl (\q s -> q |> (moves + 1, s)) restQueue validNewStates
                 in
                   go newQueue newVisited

  in
    -- Check if the very first state (after filling bucket 1) is the goal
    if fst initialState == target
      then Just (initialMoves, (1, snd initialState))
      else go initialQueue initialVisited

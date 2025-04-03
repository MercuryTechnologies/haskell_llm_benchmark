module TwoBucket (measure) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)), (<|), (|>)) -- Import specific constructors/operators for Queue
import qualified Data.Set as Set
import Data.Set (Set) -- Explicitly import Set type

-- State representation: (liters_in_bucket1, liters_in_bucket2)
type State = (Int, Int)
-- Queue element: (moves, state)
type QueueElement = (Int, State)
-- Visited set stores states we've already processed
type Visited = Set State

-- | Solves the two-bucket problem.
-- Given the capacities of two buckets, a target amount, and assuming we start by filling bucket one,
-- determines the minimum number of moves to reach the target, the final state of the buckets,
-- and which bucket contains the target.
-- Returns Nothing if the target is impossible to reach.
measure :: (Int, Int) -- ^ (capacity of bucket one, capacity of bucket two)
        -> Int        -- ^ target liters
        -> Maybe (Int, (Int, Int)) -- ^ Just (total_moves, (final_liters_b1, final_liters_b2)) or Nothing
measure (cap1, cap2) target
    -- Basic impossibility checks:
    -- Target is non-positive, larger than both buckets, or not measurable (GCD condition).
    -- The GCD check assumes positive capacities.
    | target <= 0 || target > max cap1 cap2 || (cap1 > 0 && cap2 > 0 && target `rem` gcd cap1 cap2 /= 0) = Nothing
    -- Optimization: If target is the capacity of the starting bucket (bucket 1), it takes 1 move.
    | target == cap1 = Just (1, (cap1, 0))
    -- Otherwise, perform Breadth-First Search
    | otherwise = bfs (cap1, cap2) target

-- | Performs Breadth-First Search to find the shortest sequence of moves.
bfs :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
bfs capacities@(cap1, cap2) target =
    -- Start state: Bucket one is filled (Move 1).
    let startState = (cap1, 0)
        startMoves = 1
        initialQueue = Seq.singleton (startMoves, startState)
        initialVisited = Set.singleton startState
        -- The state that is forbidden to reach after an action, assuming bucket 1 was the designated start bucket.
        forbiddenState = (0, cap2)
    in
        -- Handle edge case: If the starting state already contains the target in the *other* bucket.
        -- This happens if target == 0, which is ruled out by initial checks, or if cap1 == 0 (also unlikely given problem constraints).
        -- The main check `target == cap1` handles the target being in the first bucket initially.
        if snd startState == target then Just (startMoves, startState)
        else go initialQueue initialVisited forbiddenState
  where
    -- The main BFS loop
    go :: Seq.Seq QueueElement -> Visited -> State -> Maybe (Int, (Int, Int))
    go queue visited forbidden
        -- If the queue is empty, the target was not reachable.
        | Seq.null queue = Nothing
        | otherwise =
            -- Dequeue the next state to explore ((moves, state), remaining_queue)
            let ((moves, currentState@(b1, b2)), restQueue) = case Seq.viewl queue of
                                                                Seq.EmptyL -> error "Impossible: Queue checked for null"
                                                                element :<| remaining -> (element, remaining)
            in
                -- Goal check: Did we reach the target in either bucket?
                if b1 == target then Just (moves, (b1, b2))
                else if b2 == target then Just (moves, (b1, b2))
                else
                    -- Generate all valid, non-forbidden next states from the current state.
                    let nextStates = generateNextStates capacities currentState forbidden
                        -- Add newly discovered states to the queue and visited set.
                        (newQueue, newVisited) = foldl (enqueueIfNew restQueue visited (moves + 1)) (restQueue, visited) nextStates
                    -- Continue the search with the updated queue and visited set.
                    in go newQueue newVisited forbidden

-- | Helper function to add a state to the queue and visited set if it hasn't been visited yet.
enqueueIfNew :: Seq.Seq QueueElement -- ^ The current queue (without the element being processed)
             -> Visited            -- ^ The set of visited states
             -> Int                -- ^ The number of moves to reach the nextState
             -> (Seq.Seq QueueElement, Visited) -- ^ Accumulator: (current queue, current visited set)
             -> State              -- ^ The potential next state to add
             -> (Seq.Seq QueueElement, Visited) -- ^ Updated (queue, visited set)
enqueueIfNew _baseQueue visited moves (currentQueue, currentVisited) nextState
    -- If the state has already been visited, do nothing.
    | Set.member nextState currentVisited = (currentQueue, currentVisited)
    -- Otherwise, add it to the end of the queue and mark it as visited.
    | otherwise = (currentQueue |> (moves, nextState), Set.insert nextState currentVisited)

-- | Generates all possible next states from a given state, applying the 6 possible actions,
--   and filtering out the forbidden state.
generateNextStates :: (Int, Int) -- ^ Capacities (cap1, cap2)
                   -> State      -- ^ Current state (b1, b2)
                   -> State      -- ^ Forbidden state
                   -> [State]    -- ^ List of valid next states
generateNextStates (cap1, cap2) (b1, b2) forbiddenState =
    filter (/= forbiddenState) $ -- Exclude the forbidden state
        [ -- Action 1: Fill bucket 1
          (cap1, b2)
          -- Action 2: Fill bucket 2
        , (b1, cap2)
          -- Action 3: Empty bucket 1
        , (0, b2)
          -- Action 4: Empty bucket 2
        , (b1, 0)
          -- Action 5: Pour bucket 1 into bucket 2
        , let pourAmount12 = min b1 (cap2 - b2) -- Amount that can be poured
          in (b1 - pourAmount12, b2 + pourAmount12)
          -- Action 6: Pour bucket 2 into bucket 1
        , let pourAmount21 = min b2 (cap1 - b1) -- Amount that can be poured
          in (b1 + pourAmount21, b2 - pourAmount21)
        ]
    -- Note: We don't explicitly filter out the current state `(b1, b2)` here,
    -- as the `enqueueIfNew` function prevents re-adding visited states, including the current one.

module TwoBucket (measure) where

import qualified Data.Set as S

data Bucket = B1 | B2 deriving (Eq, Show)

data State = State { b1 :: Int, b2 :: Int, actions :: Int, lastAction :: Maybe Bucket } deriving (Eq, Show)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (b1Size, b2Size) target = 
    let initialState = State { b1 = 0, b2 = 0, actions = 0, lastAction = Nothing }
        queue = [(initialState, B1)]
    in case bfs queue S.empty b1Size b2Size target of
        Nothing -> Nothing
        Just (actions, b1Final, b2Final) -> Just (actions, (b1Final, b2Final))

bfs :: [(State, Bucket)] -> S.Set State -> Int -> Int -> Int -> Maybe (Int, Int, Int)
bfs [] _ _ _ _ = Nothing
bfs ((state, startBucket):queue) visited b1Size b2Size target
    | S.member state visited = bfs queue visited b1Size b2Size target
    | b1 state == target = Just (actions state, b1 state, b2 state)
    | b2 state == target = Just (actions state, b1 state, b2 state)
    | otherwise = 
        let nextStates = getNextStates state startBucket b1Size b2Size
            newQueue = queue ++ [(s, startBucket) | s <- nextStates]
            newVisited = S.insert state visited
        in bfs newQueue newVisited b1Size b2Size target

getNextStates :: State -> Bucket -> Int -> Int -> [State]
getNextStates state startBucket b1Size b2Size = 
    let fillB1 = State { b1 = b1Size, b2 = b2 state, actions = actions state + 1, lastAction = Just B1 }
        fillB2 = State { b1 = b1 state, b2 = b2Size, actions = actions state + 1, lastAction = Just B2 }
        emptyB1 = State { b1 = 0, b2 = b2 state, actions = actions state + 1, lastAction = Just B1 }
        emptyB2 = State { b1 = b1 state, b2 = 0, actions = actions state + 1, lastAction = Just B2 }
        pourB1ToB2 = pour state B1 B2 b1Size b2Size
        pourB2ToB1 = pour state B2 B1 b1Size b2Size
        invalidState = case lastAction state of
            Just B1 -> State { b1 = 0, b2 = b2Size, actions = 0, lastAction = Nothing }
            Just B2 -> State { b1 = b1Size, b2 = 0, actions = 0, lastAction = Nothing }
            Nothing -> State { b1 = -1, b2 = -1, actions = 0, lastAction = Nothing }
    in filter (\s -> s /= invalidState) [fillB1, fillB2, emptyB1, emptyB2, pourB1ToB2, pourB2ToB1]

pour :: State -> Bucket -> Bucket -> Int -> Int -> State
pour state src dst b1Size b2Size = 
    let (srcVol, dstVol) = if src == B1 then (b1 state, b2 state) else (b2 state, b1 state)
        maxDstVol = if dst == B1 then b1Size else b2Size
        transferVol = min srcVol (maxDstVol - dstVol)
        newSrcVol = srcVol - transferVol
        newDstVol = dstVol + transferVol
        (newB1, newB2) = if src == B1 then (newSrcVol, newDstVol) else (newDstVol, newSrcVol)
    in State { b1 = newB1, b2 = newB2, actions = actions state + 1, lastAction = Just src }

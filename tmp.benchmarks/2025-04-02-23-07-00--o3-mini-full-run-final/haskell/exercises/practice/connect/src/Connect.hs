module Connect (Mark(..), winner) where

import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner boardLines =
    let board = map words boardLines
        numRows = length board
        numCols = if null board then 0 else length (head board)
        -- Check paths: Player O (token 'O') must connect top to bottom.
        oWin = existsPath board numRows numCols 'O' (\r _ nr _ -> nr == numRows - 1) (\r c -> r == 0)
        -- Check paths: Player X (token 'X') must connect left to right.
        xWin = existsPath board numRows numCols 'X' (\_ c _ nc -> nc == numCols - 1) (\r c -> c == 0)
    in if oWin && not xWin then Just Nought
       else if xWin && not oWin then Just Cross
       else Nothing

-- existsPath does a DFS starting from all cells that satisfy the 'isStart' predicate.
-- Once a path reaches a cell that satisfies 'isTarget', the traversal succeeds.
existsPath :: [[String]]
           -> Int
           -> Int
           -> Char
           -> (Int -> Int -> Int -> Int -> Bool)
           -> ((Int, Int) -> Bool)
           -> Bool
existsPath board numRows numCols token isTarget isStart =
    let startingNodes = [(r, c) | r <- [0 .. numRows - 1],
                                  c <- [0 .. numCols - 1],
                                  isStart (r, c),
                                  getCell board r c == [token]]
    in any (\node -> dfs board numRows numCols token node isTarget Set.empty) startingNodes

dfs :: [[String]]
    -> Int
    -> Int
    -> Char
    -> (Int, Int)
    -> (Int -> Int -> Int -> Int -> Bool)
    -> Set.Set (Int, Int)
    -> Bool
dfs board numRows numCols token (r, c) isTarget visited
    | isTarget r c numRows numCols = True
    | Set.member (r, c) visited = False
    | otherwise =
        let visited' = Set.insert (r, c) visited
            neighs = neighbors r c
            validNeighs = filter (\(nr, nc) ->
                                    nr >= 0 && nr < numRows &&
                                    nc >= 0 && nc < numCols &&
                                    getCell board nr nc == [token]
                                 ) neighs
        in any (\n -> dfs board numRows numCols token n isTarget visited') validNeighs

neighbors :: Int -> Int -> [(Int, Int)]
neighbors r c =
    [ (r-1, c)   -- up-left
    , (r-1, c+1) -- up-right
    , (r, c-1)   -- left
    , (r, c+1)   -- right
    , (r+1, c-1) -- down-left
    , (r+1, c)   -- down-right
    ]

getCell :: [[String]] -> Int -> Int -> String
getCell board r c = (board !! r) !! c

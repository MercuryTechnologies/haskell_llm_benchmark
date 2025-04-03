module Dominoes (chain) where

import qualified Data.Map as Map
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Compute a way to order a given set of dominoes in such a way that they form
--   a correct domino chain (adjacent stones match), and the first and last
--   numbers are the same.
--
--   Return Just the re-ordered list of dominoes (with orientation) if possible.
--   Return Nothing if no valid chain exists.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes
  | null dominoes = Just []
  | otherwise =
      let adj = buildAdj dominoes
          deg = Map.map sum adj
          -- collect vertices that actually appear in edges
          nonIso = [v | (v,d) <- Map.toList deg, d > 0]
      in
         -- For an Eulerian cycle, each vertex with edges must have an even degree.
         -- We also need the graph to be connected ignoring isolated vertices.
         if all even (Map.elems deg) && isConnected adj nonIso
            then
              let start = head nonIso
                  cyclePath = eulerianCycle adj start
                  chainResult = buildChain cyclePath
              in Just chainResult
            else Nothing

-- Build a map of adjacency counts. For each stone (a,b), increment
-- adjacency[a][b] and adjacency[b][a].
buildAdj :: [(Int, Int)] -> Map.Map Int (Map.Map Int Int)
buildAdj = foldr insertStone Map.empty
  where
    insertStone (a,b) acc =
      let incr x y = Map.insertWith (+) y 1
          update x y = Map.insertWith (Map.unionWith (+)) x (Map.singleton y 1)
      in update a b (update b a acc)

-- Check connectivity ignoring isolated vertices using a simple BFS.
isConnected :: Map.Map Int (Map.Map Int Int) -> [Int] -> Bool
isConnected _ [] = True
isConnected adj (start:_) =
  let visited = bfs adj [start] []
      -- Only consider vertices that actually have edges (degree > 0).
      hasEdges v = Map.member v adj && sum (adj Map.! v) > 0
      allNonIso = [v | v <- Map.keys adj, hasEdges v]
  in length visited == length allNonIso

bfs :: Map.Map Int (Map.Map Int Int) -> [Int] -> [Int] -> [Int]
bfs _ [] visited = visited
bfs adj (x:xs) visited
  | x `elem` visited = bfs adj xs visited
  | otherwise =
      case Map.lookup x adj of
        Nothing -> bfs adj xs (x : visited)
        Just mp ->
          let neighbors = Map.keys mp
          in bfs adj (xs ++ neighbors) (x : visited)

-- Construct an Eulerian cycle using Hierholzer's algorithm.
-- Returns a list of vertices in the cycle, including repetition of the start vertex at the end.
eulerianCycle :: Map.Map Int (Map.Map Int Int) -> Int -> [Int]
eulerianCycle adjacency start = go adjacency [start] []
  where
    go :: Map.Map Int (Map.Map Int Int) -> [Int] -> [Int] -> [Int]
    go adj [] path = path
    go adj (v:stack) path
      | hasEdges v adj =
          let u = head (Map.keys (adj Map.! v))  -- pick any neighbor
              adj' = removeEdge v u adj
          in go adj' (u : v : stack) path
      | otherwise = go adj stack (v : path)

    hasEdges :: Int -> Map.Map Int (Map.Map Int Int) -> Bool
    hasEdges v adj = MaybeNotNull (Map.lookup v adj)

    MaybeNotNull Nothing  = False
    MaybeNotNull (Just m) = not (Map.null m)

-- Remove a single undirected edge (a,b) from the adjacency map.
removeEdge :: Int -> Int -> Map.Map Int (Map.Map Int Int) -> Map.Map Int (Map.Map Int Int)
removeEdge a b mp = removeOne b a (removeOne a b mp)
  where
    removeOne x y m =
      case Map.lookup x m of
        Nothing -> m
        Just edges ->
          case Map.lookup y edges of
            Nothing -> m
            Just count ->
              let count' = count - 1
                  edges' = if count' <= 0 then Map.delete y edges
                                          else Map.insert y count' edges
              in if Map.null edges'
                 then Map.delete x m
                 else Map.insert x edges' m

-- Convert the list of vertices from the Eulerian cycle into a list of oriented dominoes.
buildChain :: [Int] -> [(Int, Int)]
buildChain [] = []
buildChain [_] = []
buildChain (x:y:zs) = (x, y) : buildChain (y : zs)

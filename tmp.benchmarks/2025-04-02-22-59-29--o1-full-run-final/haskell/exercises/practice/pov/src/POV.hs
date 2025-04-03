module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.List (nub)

-- | Build an undirected adjacency list from a Tree.
--   Each node is keyed to the list of its neighbors.
buildAdj :: Eq a => Tree a -> [(a, [a])]
buildAdj = go []
  where
    go acc (Node r cs) =
      -- Ensure the current node is recorded, even if it has no children
      let accWithR = ensureNode r acc
          -- Insert edges between r and each of its subtree roots
          acc' = foldl (\m (Node childLabel _) ->
                          addEdge childLabel r (addEdge r childLabel m))
                       accWithR
                       cs
      in foldl go acc' cs

    ensureNode x assoc =
      case lookup x assoc of
        Just _  -> assoc
        Nothing -> (x, []):assoc

    -- Insert y in x's adjacency list, avoiding duplicates
    addEdge x y [] = [(x,[y])]
    addEdge x y ((k,vs):rest)
      | k == x    = (k, nub (y:vs)) : rest
      | otherwise = (k,vs) : addEdge x y rest

-- | Lookup adjacency list for a node
lookupAdj :: Eq a => a -> [(a,[a])] -> [a]
lookupAdj x assoc = case lookup x assoc of
  Just ns -> ns
  Nothing -> []

-- | Check if x occurs in the adjacency map
inAdj :: Eq a => a -> [(a,[a])] -> Bool
inAdj x assoc = case lookup x assoc of
  Just _  -> True
  Nothing -> False

-- | Build a new Tree rooted at 'node', ignoring 'parent' to avoid cycles.
mkTree :: Eq a => a        -- ^ current node
       -> Maybe a          -- ^ parent (avoid going backward)
       -> [(a,[a])]        -- ^ adjacency
       -> Tree a
mkTree node parent adj =
  let children = [ c | c <- lookupAdj node adj, Just c /= parent ]
  in Node node (map (\ch -> mkTree ch (Just node) adj) children)

-- | Re-root the Tree on the node 'x'.
--   If 'x' not in original tree, returns Nothing.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x original =
  let adjacency = buildAdj original
  in if not (inAdj x adjacency)
     then Nothing
     else Just (mkTree x Nothing adjacency)

-- | Find a path from the root of a Tree to a target node.
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath x (Node r children)
  | x == r    = Just [x]
  | otherwise = tryChildren children
  where
    tryChildren [] = Nothing
    tryChildren (c:cs) =
      case findPath x c of
        Just path -> Just (r : path)
        Nothing   -> tryChildren cs

-- | Re-root the tree from the perspective of 'from',
--   then get the path between 'from' and 'to'.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to original = do
  reRooted <- fromPOV from original
  findPath to reRooted

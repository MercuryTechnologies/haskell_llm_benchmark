module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.Maybe (mapMaybe, listToMaybe)
-- No longer need Data.List for filter

-- Helper for fromPOV: Finds the target node 'x' and restructures the path above it.
-- Returns Maybe (target node with original children, list of ancestor nodes)
-- Ancestor nodes contain only their children *not* on the path to x.
-- The ancestor list is ordered [parent, grandparent, ..., original_root].
findAndRestructure :: Eq a => a -> Tree a -> Maybe (Tree a, [Tree a])
findAndRestructure x (Node root children)
  | root == x = Just (Node x children, []) -- Found target: return node with its children, empty ancestor list
  | otherwise =
      -- Recursively search in children. Use mapMaybe to handle failures.
      -- We expect to find the target in at most one child branch in a valid tree.
      case mapMaybe (\child -> findAndRestructure x child >>= \(found, ancestors) -> Just (child, found, ancestors)) children of
        [] -> Nothing -- Target not found in any child branch
        [(childTree, foundSubtree, ancestors)] -> -- Target found in exactly one child branch
            -- 'childTree' is the original subtree where 'x' was found.
            -- 'foundSubtree' is the node 'x' with its original children.
            -- 'ancestors' is the list built so far from nodes between 'childTree' and 'x'.

            -- Create the current node as an ancestor, including only its children that are siblings of the path.
            -- Filter out the child branch ('childTree') where 'x' was found by comparing root labels.
            let currentAncestor = Node root (filter (\node -> rootLabel node /= rootLabel childTree) children)
            -- Return the found subtree and append the current ancestor to the list.
            in Just (foundSubtree, ancestors ++ [currentAncestor]) -- Append instead of prepend
        _ -> error "Target node found in multiple children or tree structure invalid." -- Or return Nothing if duplicates are possible? Instructions imply a valid tree.

-- Helper for fromPOV: Builds the ancestor chain into a single tree branch.
-- Input: [parent, grandparent, ..., original_root] (ancestor nodes have non-path children)
-- Output: Tree representing parent -> grandparent -> ... -> original_root, with siblings attached correctly.
buildAncestorChain :: [Tree a] -> Maybe (Tree a)
buildAncestorChain [] = Nothing -- No ancestors
buildAncestorChain [lastAncestor] = Just lastAncestor -- The original root becomes a leaf in the chain (or root if only one ancestor)
buildAncestorChain (currentAncestor : remainingAncestors) = do
    -- Recursively build the chain for the ancestors further up (grandparent onwards)
    maybeNextAncestorChain <- buildAncestorChain remainingAncestors
    -- Attach the next part of the chain as a child to the current ancestor,
    -- alongside its original non-path children (siblings).
    case maybeNextAncestorChain of
       -- This case should ideally not happen if remainingAncestors is non-empty,
       -- but handles it by returning the current ancestor as the chain end.
       Nothing -> Just currentAncestor
       Just nextAncestorChain -> Just $ Node (rootLabel currentAncestor) (nextAncestorChain : subForest currentAncestor)


-- | Re-orients the tree from the perspective of the given node.
-- If the node is not in the tree, returns Nothing.
fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
    -- 1. Find the target node and the list of ancestors with their sibling branches.
    (targetNodeWithChildren, ancestors) <- findAndRestructure x tree
    -- targetNodeWithChildren is (Node x xChildren)
    -- ancestors is [parent{pSibs}, gp{gpSibs}, ..., root{rootSibs}]

    -- 2. Build the single branch representing the ancestor path.
    let maybeAncestorChain = buildAncestorChain ancestors

    -- 3. Combine the target node and the ancestor chain.
    let (Node xRoot xChildren) = targetNodeWithChildren
    case maybeAncestorChain of
      -- If no ancestors, x was the root, return it as is.
      Nothing -> Just targetNodeWithChildren
      -- If ancestors exist, make the chain a child of x.
      Just ancestorChain -> Just $ Node xRoot (ancestorChain : xChildren)


-- Helper for tracePathBetween: Finds path from root to target in a given tree.
findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node root children)
  | root == target = Just [root] -- Target is the root
  | otherwise =
      -- Search children, get the first path found using listToMaybe.
      case listToMaybe $ mapMaybe (findPath target) children of
        Nothing -> Nothing -- Target not found in any child
        Just path -> Just (root : path) -- Prepend current root to the found path

-- | Finds the path between two nodes in the tree.
-- Returns Nothing if either node is not in the tree.
tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
    -- 1. Re-orient the tree from the 'from' node's perspective.
    newTree <- fromPOV from tree
    -- 2. Find the path from the new root ('from') to 'to' in the re-oriented tree.
    findPath to newTree

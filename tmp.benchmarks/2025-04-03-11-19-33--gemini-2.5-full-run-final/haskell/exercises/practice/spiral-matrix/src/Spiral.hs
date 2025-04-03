module Spiral (spiral) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST) -- Using runST for ST monad execution
import Data.Array.ST (STUArray, newArray, writeArray, freeze) -- For mutable arrays in ST
import Data.Array.Unboxed (UArray, bounds, (!)) -- For immutable unboxed arrays

-- Define directions for spiral movement
data Dir = R | D | L | U deriving (Eq)

-- Helper function to calculate the next position based on direction
move :: (Int, Int) -> Dir -> (Int, Int)
move (r, c) R = (r, c + 1) -- Move Right
move (r, c) D = (r + 1, c) -- Move Down
move (r, c) L = (r, c - 1) -- Move Left
move (r, c) U = (r - 1, c) -- Move Up

-- Function to convert the final UArray to the required [[Int]] format
uArrayToLists :: UArray (Int, Int) Int -> [[Int]]
uArrayToLists arr =
    let ((minR, minC), (maxR, maxC)) = bounds arr -- Get array bounds
    -- Create a list of lists (rows) by iterating through row and column indices
    in [[arr ! (r, c) | c <- [minC..maxC]] | r <- [minR..maxR]]

-- Main function to generate the spiral matrix
spiral :: Int -> [[Int]]
spiral 0 = [] -- Handle the edge case of size 0
spiral size = uArrayToLists $ runST $ do -- Run the ST computation to get a UArray
    -- Create a mutable array (0-indexed) initialized with 0s
    arr <- newArray ((0, 0), (size - 1, size - 1)) 0 :: ST s (STUArray s (Int, Int) Int)
    let total = size * size -- Total number of elements to fill

    -- Recursive loop function within the ST monad to perform the simulation
    -- State: (current_pos, current_dir, current_val, minR, maxR, minC, maxC)
    let loop (r, c) dir val minR maxR minC maxC = do
        -- Continue as long as we haven't filled all elements
        when (val <= total) $ do
            -- Write the current value to the array at the current position
            writeArray arr (r, c) val

            -- Calculate the potential next position
            let (nextR, nextC) = move (r, c) dir

            -- Check if the next position hits a boundary, change direction and shrink bounds if necessary
            if dir == R && nextC > maxC then
                -- Hit right boundary: Turn Down, increment minR (shrunk top boundary)
                loop (r + 1, c) D (val + 1) (minR + 1) maxR minC maxC
            else if dir == D && nextR > maxR then
                -- Hit bottom boundary: Turn Left, decrement maxC (shrunk right boundary)
                loop (r, c - 1) L (val + 1) minR maxR minC (maxC - 1)
            else if dir == L && nextC < minC then
                -- Hit left boundary: Turn Up, decrement maxR (shrunk bottom boundary)
                loop (r - 1, c) U (val + 1) minR (maxR - 1) minC maxC
            else if dir == U && nextR < minR then
                -- Hit top boundary: Turn Right, increment minC (shrunk left boundary)
                loop (r, c + 1) R (val + 1) minR maxR (minC + 1) maxC
            else
                -- No boundary hit: Continue in the same direction
                loop (nextR, nextC) dir (val + 1) minR maxR minC maxC

    -- Start the simulation loop:
    -- Initial position: (0, 0)
    -- Initial direction: R (Right)
    -- Initial value: 1
    -- Initial boundaries: 0 to size-1 for both rows and columns
    loop (0, 0) R 1 0 (size - 1) 0 (size - 1) -- Corrected indentation

    -- Freeze the mutable STUArray to an immutable UArray before returning from runST
    freeze arr -- Corrected indentation

-- filepath: web/MazeLogic.hs
module MazeLogic where

import System.Random (randomRIO)
import Data.List (nub)

type Cell = (Int, Int)
type Maze = [[Bool]]

-- Generates an empty maze filled with walls (False)
generateEmptyMaze :: Int -> Maze
generateEmptyMaze size = replicate size (replicate size False)

-- Generates a random maze using a simple algorithm (e.g., Prim's algorithm)
generateMaze :: Int -> IO Maze
generateMaze size = do
    let emptyMaze = generateEmptyMaze size
    primsMaze <- primAlgorithm emptyMaze [(0, 0)] []
    return $ addBorder primsMaze size

-- Example implementation of Prim's algorithm for maze generation
primAlgorithm :: Maze -> [Cell] -> [Cell] -> IO Maze
primAlgorithm maze frontier visited = do
    if null frontier
        then return maze
        else do
            index <- randomRIO (0, length frontier - 1)
            let (current, newFrontier) = removeCell index frontier
            let newMaze = setCell maze current True
            let updatedVisited = current : visited
            let newNeighbours = filter (`notElem` visited) (cellNeighbours current)
            let updatedFrontier = nub (newFrontier ++ newNeighbours)
            primAlgorithm newMaze updatedFrontier updatedVisited

-- Helper functions for maze generation
removeCell :: Int -> [a] -> (a, [a])
removeCell index list = let (before, x:after) = splitAt index list in (x, before ++ after)

setCell :: Maze -> Cell -> Bool -> Maze
setCell maze (x, y) val = take y maze ++ [row] ++ drop (y + 1) maze
  where row = take x (maze !! y) ++ [val] ++ drop (x + 1) (maze !! y)

cellNeighbours :: Cell -> [Cell]
cellNeighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
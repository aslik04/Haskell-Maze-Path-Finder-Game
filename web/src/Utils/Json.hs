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
    prims emptyMaze [(0, 0)] []
  where
    prims maze frontier visited
        | null frontier = return maze
        | otherwise = do
            index <- randomRIO (0, length frontier - 1)
            let (current, newFrontier) = removeCell index frontier
            let newMaze = setCell maze current True
            let updatedVisited = current : visited
            let newNeighbours = filter (`notElem` visited) (cellNeighbours current)
            let updatedFrontier = nub (newFrontier ++ newNeighbours)
            prims newMaze updatedFrontier updatedVisited

    removeCell index list = let (before, x:after) = splitAt index list in (x, before ++ after)
    setCell maze (x, y) val = take y maze ++ [take x (maze !! y) ++ [val] ++ drop (x + 1) (maze !! y)] ++ drop (y + 1) maze
    cellNeighbours (x, y) = [(nx, ny) | (nx, ny) <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], nx >= 0, ny >= 0]

-- Example pathfinding function (A* algorithm)
findPath :: Maze -> Cell -> Cell -> Maybe [Cell]
findPath maze start goal = Just []  -- Placeholder for actual pathfinding logic
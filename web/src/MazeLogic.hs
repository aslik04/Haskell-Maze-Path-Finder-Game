-- filepath: web/MazeLogic.hs
module MazeLogic where

import System.Random (randomRIO)
import Data.List (nub)

type Cell = (Int, Int)
type Maze = [[Bool]]

-- Generates an empty maze filled with walls (False)
generateEmptyMaze :: Int -> Maze
generateEmptyMaze size = replicate size (replicate size False)

-- Generates a maze using Prim's algorithm
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
          let newVisited = current : visited
          let newNeighbours = filter (`notElem` visited) (cellNeighbours size current)
          let oldNeighbours = filter (`elem` visited) (cellNeighbours size current)
          if length oldNeighbours > 1
              then prims newMaze newFrontier newVisited
              else do
                  let updatedMaze = if null oldNeighbours
                                    then newMaze
                                    else createPath newMaze current (head oldNeighbours)
                  let updatedFrontier = nub (newFrontier ++ newNeighbours)
                  prims (setCell updatedMaze (size - 1, size - 1) True) updatedFrontier newVisited

-- Helper functions
removeCell :: Int -> [a] -> (a, [a])
removeCell index list = let (before, x:after) = splitAt index list in (x, before ++ after)

setCell :: Maze -> Cell -> Bool -> Maze
setCell maze (x, y) val = take y maze ++ [row] ++ drop (y + 1) maze
  where row = take x (maze !! y) ++ [val] ++ drop (x + 1) (maze !! y)

cellNeighbours :: Int -> Cell -> [Cell]
cellNeighbours size (x, y) =
    filter isValid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where
    isValid (nx, ny) = nx >= 0 && nx < size && ny >= 0 && ny < size

createPath :: Maze -> Cell -> Cell -> Maze
createPath maze cell1 cell2 = setCell (setCell maze cell1 True) cell2 True
module Hurtle.PrimMazeGenerator where

import Hurtle.Types (Maze, Cell)
import System.Random (randomRIO)
import Data.List (nub)
import Hurtle.OptimalPath (search)

{- Generates a 2D list with height lists, and each list is width long, 
   all filled with False, which simulates all walls. -}
generateEmptyMaze :: Int -> Maze
generateEmptyMaze size =
    -- Initialises All cells in 2D list (Maze) to walls (False)
    replicate size (replicate size False)


{- I utilised do notation due to its favourable readability. First using let, 
   we assign emptyMaze to a newly generated empty maze of the size specified, 
   then prims is the unwrapped result of (primAlgorithm) which is neccessary in 
   order to utilise the helper function addBorder, which will add a wall 
   on either side of the maze, and lift it using pure. On the odd occassion
   a unsolveable maze would be generated, having being stuck on this issue, 
   i eventually decided to test the maze if it solveable using aStar, and if not
   recursively calling method, or returning a tuple containing maze and its 
   optimal path, this turned out to be an efficient way of doing it as i needed
   to call the search method anyway in MazeGame.hs -}
generateMaze :: Int     -- The size of the maze being generated
             -> IO (Maze, [Cell]) {- Lifted Maze of type ([[Bool]]) 
                                     and the optimal path to solve same maze -}
generateMaze size = do
    -- [[Bool]] of size dimensions, filled with False
    let emptyMaze = generateEmptyMaze size
    -- Swaps some of these False indexes with True, using Prim Algorithm
    prims <- primAlgorithm emptyMaze size [(0, 0)] []
    -- Tests if maze is solveable, first adds walls to maze
    let testMaze = addBorder prims size
    -- Maybe list of cells, containing optimal path
    case search testMaze (1, 1) (size, size) of
        -- If maze is solveable then is returned as tuple with maze
        Just path -> pure (testMaze, path)
        -- Recursively call the method to keep trying until solveable maze 
        Nothing -> generateMaze size


{- Utilising let in notation to clearly display how the borders are being created, 
   then showing the reader how, it is appended to the maze 2D list ([[Bool]]). 
   A horizontal border, is 2 larger than original maze size, as it adds a wall either side, 
   therefore the top and bottom walls will be 2 larger too, and filled with false. 
   Utilising map for concise and elegant mapping, each list in the maze, has a
   False value appended to start and end, which adds a wall to left and right. 
   This is finalised by added the horizontall wall to start and end of 2D list. -}
addBorder :: Maze -- PrimAlgorith maze with no border
          -> Int  -- Size of maze border is being added to
          -> Maze -- Fully formed Maze with walls on all 4 sides
addBorder maze size =
    -- Creates a list 2 larger than size, and filled with false, for top and bottom
    let horizontalBorder = replicate (size + 2) False
        -- Adds False to start and end of a row(list), adding wall to left and right
        verticalBorder row = [False] ++ row ++ [False]
    -- Each list in the maze is mapped to have a wall left and right, 
    -- Then a horizontalBorder is added to top and bottom
    in horizontalBorder : map verticalBorder maze ++ [horizontalBorder]


{- Utilised pattern matching to identify 2 cases in primAlgorithm, where frontier
   has been exhausted, return the lifted maze, else call the algorithm. 
   First we choose a random frontier cell, these are a list of cells, that are orthogonal
   neighbours to cells which are part of the maze(have been set to True). After selecting
   a random frontier it is removed from the frontier list, this is what allows the recursive
   nature of the function to not endlessly loop. Frontier cells are candidates to be added
   to the maze, by (setting cell to True) Through utilisation of filter, a set 
   of neighbours not visited, and set of visited neighbours are created. To avoid loops, 
   only cells with 1 or less neighbours already part of the maze, if true this current cell
   is added to the maze by setting its coordinate to True, and frontier is updated along with
   visited list, then recursively calling primAlgorithm to exhaust the frontier and carve
   a continuous path to the final cell. My implementation ensures that there is no loops, 
   and exists only one connected path between start and end (0, 0) and (size - 1, size - 1) -}
primAlgorithm :: Maze    -- Empty maze which is filled with False values
              -> Int     -- Size of the empty maze
              -> [Cell]  -- Cells not part of maze but neighbouring cells which are part of the maze 
              -> [Cell]  -- Cells which have already been visited during recursive calls
              -> IO Maze -- Lifted maze, after the frontier list has been exhausted
primAlgorithm maze _ [] _ = pure maze
primAlgorithm maze size frontier visited = do
    -- Choose random index in frontier list
    index <- randomRIO (0, length frontier - 1)
    {- Remove selected element from the frontier list and assign to new frontier
       current is the value of the index which was randomly selected and removed -}
    let (current, newFrontier) = removeCell index frontier
        {- Retrieve the orthogonal neighbours to current cell, not in visitied list, 
           these could potentially be added to frontier list. -}
        newNeighbour = filter (`notElem` visited) $ cellNeighbours size current
        -- Retrieve orthogonal neighbours to current cell, which is in visited list 
        oldNeighbour = filter (`elem` visited) $ cellNeighbours size current
    -- If current cell has more than one neighbour in visited list
    if length oldNeighbour > 1
        -- if true, recursively call primAlgorithm , this avoids loops
        then primAlgorithm maze size newFrontier visited
        -- In the case current cell has 1 or less neighbours in visited list
        else do
            -- Starting a path to oldNeighbour, by setting current cell to True(removing wall)
            let mazeWithCurrent = setCell maze current True
            -- Adds current cell to list of visited cells
            let updatedVisited = current : visited
            -- Value of new maze depending on number of neighbours in visited list
            let newMaze = if null oldNeighbour
                -- If current cell has no oldNeighbours, newMaze is maze with current wall removed
                then mazeWithCurrent
                -- Otherwise create a path using the first(head) old neighbour
                else createPath mazeWithCurrent current (head oldNeighbour)
            -- Any new neighbours which we did not visit are added to frontier list.
            let updatedFrontier = nub $ newFrontier ++  newNeighbour
            -- Ensures end cell is always true
            let newMazeWithEnd = setCell newMaze (size - 1, size - 1) True
            -- Recursively called until frontier list is exhausted
            primAlgorithm newMazeWithEnd size updatedFrontier updatedVisited


{- Utilising case of notation to improve readability, and clearly show the 
   case where index can be removed, and give an error if the index can not be removed.  
   (splitAt) is used to retrieve the elements before an after a specified index, 
   by returning the list, where the elements before and after list are appended, 
   this removes any reference to the element at the index we wanted to remove. 
   Otherwise print an error message as index couldnt be found.  -}
removeCell :: Int      -- Position in list we want to remove
           -> [a]      -- The list element is being removed from
           -> (a, [a]) -- List after the element has been removed 
removeCell index list = case splitAt index list of
    -- List displaying elements before and after desired index
    (before, x:after) -> (x, before ++ after)
    -- Otherwise index could not be found in specified list.
    _ -> error "Index is out of bounds"


{- Utilising where to dislay the helper function which evaluates if the neighbour is valid, 
   List of the cells orthogonal neighbours are filtered through, which removes an element, 
   if the helper function (checkNeighbour) returns false for that specifc coordinate. 
   Elements which x/y coordinates are not between 0(inclusive) and size are  invalid
   as they are off the scope of the maze, and should not be considered during prims algorithm -}
cellNeighbours :: Int    -- Size of maze we are finding valid neighbours for coordinate in
               -> Cell   -- Coordinate we are looking for valid neighbours for (Int, Int)
               -> [Cell] -- A list of valid neighbour coordinates [(Int, Int)]
cellNeighbours size (x, y) =
    -- Each possible orthogonal neighbour is checked if valid using helper function. 
    filter checkNeighbour [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    where
        -- Neighbour is valid if within the bounded index of the maze
        checkNeighbour (nx, ny) = nx >= 0 && nx < size && ny >= 0 && ny < size


{- Utilising where notation to effectively define a helper function which improves readability, 
   overall technique of this function is to retrieve elements before and after target x
   and y respectively, and by doing this we can append before and after to the 
   values we want to set to.  -}
setCell :: Maze -- Maze(2D [[Bool]]) where cells value is being altered 
        -> Cell -- The coordinate in Maze where value is being changed
        -> Bool -- The value which cell is being changed to
        -> Maze -- The Maze after the cell has been changed
setCell maze (x, y) val =
    {- Effectively splitting the maze at the cells coordinate, this is done by using 
    (take) to keep rows before target row, appended by the row, after cell has been set,
    and drop to get rows after target row. -}
    take y maze ++ [row] ++ drop (y + 1) maze
    where
        {- takes elements before target cells x coordinate, (maze !! y), in order to
        directly access the list desired cell is in, the desired value is appended 
        to values before target cell, and using drop to append values after target -}
        row = take x (maze !! y) ++ [val] ++ drop (x + 1) (maze !! y)


{- Straightforward function which first sets the coordinate of the first cell, 
   in the maze to True, and then using the result of this sets the second coordinate 
   to true, and then returns the result of this. Utilises the previously defined 
   setCell function which changes the value of the coordinate to True. -}
createPath :: Maze -- The maze we are removing walls from to 'create' a path
           -> Cell -- Coordinates of first cell in path, set to true
           -> Cell -- Coordinates of second cell in path, set to true
           -> Maze -- Maze after walls have been removed (set to True)
createPath maze cell1 cell2 = setCell (setCell maze cell1 True) cell2 True
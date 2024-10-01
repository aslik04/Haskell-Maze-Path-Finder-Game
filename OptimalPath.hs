module Hurtle.OptimalPath where

import Hurtle.Types ( Node (currentPos, gCost, fCost, parent, Node)
                    , Maze
                    , Cell )
import Data.List (sortOn, delete)


{- Calculates distance between 2 points, called manhattan distance, 
   used to calculate the hCost for a node. abs used to remove possible
   negatives as we only need the difference between 2 points -}
manhattanDistance :: Cell -- first coordinates distance calculated between
                  -> Cell -- second coordinates distance calculated between 
                  -> Int  -- disance between 2 coordinates
manhattanDistance (x1, y1) (x2, y2) =
    -- absolute difference between 2 coordinates
    abs (x1 - x2) + abs (y1 - y2)


{- I utilised let in notation due to its readability, and allows me to clearly display, 
   the helper functions before i declare the result of searching a maze. The wrapper
   of a starting node is first initialised to represent the starting point in a maze, 
   The node after aStar is applied to the maze, is also initialised, this node 
   contains references from the start all the way to the goal of a unrefined path
   therefore we use fmap to (formPath), which forms the optimal path of maze.
   (<$>) is vital for elegance and readibility as it seamlessly handles when 
   an optimal path cant be found and thus only applies funtion if contains a value -}
search :: Maze         -- Maze which is going to be searched for optimal path
       -> Cell         -- Starting position in maze
       -> Cell         -- goal position in maze
       -> Maybe [Cell] -- Maybe list of coordinates of optimal path or Nothin
search maze start goal =
    {- starting node to begin search in maze, gCost set to 0, as at start point, 
       and manhattan distance between start and goal is for hCost -}
    let startNode = Node start 0 (manhattanDistance start goal) Nothing
        {- node which contains all links to optimal path through parent node
           which links all the way down to the start node to form a path -}
        goalNode = aStarAlgorithm maze [startNode] [] goal
    {- (.) function composition firs the goalNodes path is formed using (formPath) 
       (<$>) utilised to elegantly handle case where goalNode is Nothing -}
    in formPath <$> goalNode


{- aStar searching algorithm implemmentation 
   Function used to find shortest path from start to goal in a maze, and uses aStar algorithm
   Iteratively searches nodes inside openList, sorted by their distance to goal node(heuristic), 
   and the known distance from the start to form fCost, this is how nodes are prioritised. 
   As nodes are explroed they are removed from open list and added to (visited) list, this
   prevents the same node being searched twice. Hellper function evaluateNeighbour assesses a neighbour
   if they should be included in openlist(list of to be explored nodes). Method recursively applied
   until the open list is exhausted or the goal has been reached.  -}
aStarAlgorithm :: Maze       -- 2D list of the maze, True represents path, and False wall
               -> [Node]     {- Nodes to be explored, dynamically updated as method 
                                recursively applied, initially jjust start -}
               -> [Node]     -- Nodes already explored prevents revisiting nodes
               -> Cell       -- Tuple which represents coordinates of goal node
               -> Maybe Node -- Just node if path to goal found, or Nothing if maze unsolveable
-- If openList was exhausted and couldnt find goal, therefore maze is unsolveable
aStarAlgorithm _ [] _ _ = Nothing
aStarAlgorithm maze openList visited goal =
    -- Initialises a sorted openlist, based on their f cost, which is distance to goal coord
    let openListSorted = sortOn fCost openList
        -- selects the node which is the lowest cost (least manhattan distance to goal)
        bestCost = head $ openListSorted
        -- the selected node is deleted from the open list and newOpenList for later recursion formed
        newOpenList = delete bestCost openListSorted
    -- Checks if node is currently at the goal, if so search is complete
    in if currentPos bestCost == goal
        {- If True return the bestcost node (as it will be the one at the goal)
           and it will now contain references to all the optimal paths.  -}
        then Just bestCost
        -- If goal hasnt been reached 
        else
            -- Retrieves a list of the explorable neighbours to our bestcosting node
            let neighbours = cellNeighbours (currentPos bestCost) maze
                {- Use of foldl which iteratively applies evaluateNeighbour and accumulates the results
                   sequentially using each element in the neighbours list. Helper function evaluateNeighbour
                   used which evaluates if open list should add a neighbour or if a better path exists -}
                updatedOpenList = foldl (evaluateNeighbour bestCost goal visited) newOpenList neighbours
                -- appends visited list to mark bestCost as explored, preventing loops
                updatedvisited = bestCost : visited
            -- Recursively searches with the updates lists, until openlist exhausted or goal found. 
            in aStarAlgorithm maze updatedOpenList updatedvisited goal


{- Helper function which evaluates if a neighbour coordinate should be added to the open list, 
   for future exploration based on potential to a more efficient path to the goal. 
   Function utilises if notation as a concise and simplified way to display that we are evaluating
   if the neighbour has already been evaluated. Where notation for readbility, allows us to 
   define the various helper function associated with evaluating a neighbour. Use of any, 
   which evaluates all elements in appendedlist, to see if its coordinate matches with coords, 
   and thus exists.  -}
evaluateNeighbour :: Node   -- Node used to evaluate neighbour, used as basis for calculating gCost
                  -> Cell   -- Coordinates of the goal in the maze, used for hCost calculation
                  -> [Node] -- list of already explored nodes
                  -> [Node] -- list of open nodes, potential to be updated with neighbour or new cost
                  -> Cell   -- Coordinates of the neighbor, which is being evaluated
                  -> [Node] {- Returns updated open list, may include evaluated neighbour if not already
                               in open or visited lists and represents more efficient path -}
evaluateNeighbour node goal visited openList neighbour =
    -- if the neighbour coordinate is in either lists
    if existsInList neighbour
        -- return the open list back, as the neighbour can not be evaluated again
        then openList
        -- else append the open list with newly created node for the neighbour
        else updatedOpen : openList
    where
        -- Checks if the neighbour is already in the visited or open lists
        existsInList coords = any ((== coords) . currentPos) (openList ++ visited)
        -- calculates gCoost of the neighbour, cost from start node to neighbour
        newG = gCost node + 1
        -- Estimation of distance to goal node, 
        newH = manhattanDistance neighbour goal
        -- generates a new node for the neighbour which now contains a reference to current node
        updatedOpen = Node neighbour newG (newG + newH) (Just node)


{- Helper function which checks if the coordinates orthogonal neighbours, are 
   inside the mazes boundaries, and the neighbours coordinate represents True 
   in the 2D maze list, utilising filter to cycle through all neighbours. -}
cellNeighbours :: Cell -- Coordinate that is being evaluated and used to form orthogonal neighbours
               -> Maze -- Maze which is being evaluated against if the neighbours are valid
               -> [Cell] -- list of all the neighbours within walls, and not walls
cellNeighbours (x, y) maze =
    {- filter applies each element in the list to the function, and if false
       is removed from the list, and all remaining elements are returned -}
    filter (neighbourValid maze) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


{- Helper function to assess if a given coordinate is within the boundaries of the 
    maze and it is not a wall and therefore contains a True value in its corresponding
    position in the 2D list maze.  -}
neighbourValid :: Maze -- Maze we are evaluating if the cooridnate are within maze and are not walls
               -> Cell -- coordinate we are checking against conditions
               -> Bool -- True if within walls, and is not a wall, False otherwise
neighbourValid maze (nx, ny) =
    {- Is maze within the bounds of maze, 0 <= x/y < size, 
       also does corresponding coordinate contain True in the 2D list maze, 
       only if all conditions are True will True be returned -}
    nx >= 0 && nx < length (head maze) && ny >= 0 && ny < length maze && maze !! ny !! nx


{- Recursive helper function which goes down all the references from goal to start, 
   all of which contain coordinates which connect the path together, to form optimal path -}
formPath :: Node   -- Node which contains references to path before it
         -> [Cell] -- List of coordinates forming optimal path
formPath node = case parent node of
    -- If node has no parent we have reached the start and therefore return current position
    Nothing -> [currentPos node]
    -- Else we append current position to recursive call to method
    Just pos -> currentPos node : formPath pos
module Web.MazeLogic
    ( generateMaze
    , solveMaze
    , Position
    , Maze
    ) where

-- | Type aliases for clarity
type Position = (Int, Int)
type Maze = [[Int]]

-- | Generate a simple maze as a 2D grid
-- 0 = path, 1 = wall
-- For now, creates a simple maze with borders and some internal walls
generateMaze :: Int -> Int -> Maze
generateMaze width height
    | width < 3 || height < 3 = [[1]] -- Minimum size fallback
    | otherwise = 
        [ [ if isBorder x y || isInternalWall x y then 1 else 0
          | x <- [0..width-1] ]
        | y <- [0..height-1] ]
  where
    -- Border walls
    isBorder x y = x == 0 || x == width-1 || y == 0 || y == height-1
    
    -- Simple internal wall pattern (creates some obstacles)
    isInternalWall x y = 
        (x `mod` 4 == 2 && y `mod` 4 == 2 && y < height - 2) ||
        (x == width `div` 2 && y > 2 && y < height - 3)

-- | Solve maze using simple pathfinding
-- Returns list of positions from start to end
-- For now, uses a simple right-hand rule approach
solveMaze :: Maze -> [Position]
solveMaze maze
    | null maze || null (head maze) = []
    | otherwise = findPath maze start end
  where
    height = length maze
    width = length (head maze)
    start = (1, 1) -- Start position (top-left open cell)
    end = (width - 2, height - 2) -- End position (bottom-right open cell)

-- | Find path from start to end using simple breadth-first search
findPath :: Maze -> Position -> Position -> [Position]
findPath maze start end = bfs [start] [] [start]
  where
    height = length maze
    width = length (head maze)
    
    -- Simple BFS implementation
    bfs :: [Position] -> [Position] -> [Position] -> [Position]
    bfs [] _ _ = [] -- No path found
    bfs (current:queue) visited path
        | current == end = reverse (current:path)
        | current `elem` visited = bfs queue visited path
        | otherwise = 
            let neighbors = getValidNeighbors current
                newQueue = queue ++ neighbors
                newVisited = current : visited
            in bfs newQueue newVisited (current:path)
    
    -- Get valid neighboring positions
    getValidNeighbors :: Position -> [Position]
    getValidNeighbors (x, y) = 
        filter isValidPosition [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
    
    -- Check if position is valid (within bounds and not a wall)
    isValidPosition :: Position -> Bool
    isValidPosition (x, y) = 
        x >= 0 && x < width && 
        y >= 0 && y < height && 
        maze !! y !! x == 0

-- | Alternative simple solver that just creates a straight path
-- (fallback if BFS fails)
simplePath :: Maze -> [Position]
simplePath maze = 
    let height = length maze
        width = if null maze then 0 else length (head maze)
    in if width > 2 && height > 2
       then [(x, 1) | x <- [1..width-2]] ++ [(width-2, y) | y <- [2..height-2]]
       else []
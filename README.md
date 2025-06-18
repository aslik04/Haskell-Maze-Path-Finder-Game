### Step 1: Create the Project Structure

1. Navigate to your `web/` directory.
2. Create a new file named `MazeLogic.hs`.
3. Ensure you have the necessary directory structure for your Haskell project.

### Step 2: Implement `MazeLogic.hs`

In `web/MazeLogic.hs`, implement pure functions for maze generation and pathfinding. Below is a basic example of how you might structure this module:

```haskell
-- filepath: web/MazeLogic.hs
module MazeLogic where

import Hurtle.Types (Maze, Cell)
import Hurtle.PrimMazeGenerator (generateMaze) -- Assuming you have this module
import Hurtle.OptimalPath (search) -- Assuming you have this module
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as B

-- Function to generate a maze of a given size
generateMazeLogic :: Int -> IO (Maze, [Cell])
generateMazeLogic size = generateMaze size

-- Function to solve a maze given in JSON format
solveMazeLogic :: String -> Maybe [Cell]
solveMazeLogic jsonMaze = 
    case decode (B.pack jsonMaze) of
        Just (maze :: Maze) -> Just (search maze (1, 1) (length maze - 1, length maze - 1))
        Nothing -> Nothing

-- Function to convert maze to JSON
mazeToJson :: Maze -> String
mazeToJson = B.unpack . encode

-- Function to convert JSON to maze
jsonToMaze :: String -> Maybe Maze
jsonToMaze json = decode (B.pack json)
```

### Step 3: Update `WebMain.hs`

In `web/WebMain.hs`, export the functions from `MazeLogic.hs` for GHCJS. Here’s how you can do that:

```haskell
-- filepath: web/WebMain.hs
module WebMain where

import MazeLogic
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B

foreign export javascript generateMazeJS :: Int -> String
generateMazeJS size = B.unpack $ encode =<< generateMazeLogic size

foreign export javascript solveMazeJS :: String -> String
solveMazeJS jsonMaze = 
    case solveMazeLogic jsonMaze of
        Just path -> B.unpack $ encode path
        Nothing -> "[]"
```

### Step 4: Ensure JSON Compatibility

Make sure you have the `aeson` library in your project dependencies to handle JSON encoding and decoding. You can add it to your `.cabal` file or `stack.yaml` file depending on your build tool.

### Step 5: Build and Test

1. Build your project using `cabal build` or `stack build`.
2. Test the functions in GHCJS to ensure they work correctly when called from JavaScript.

### Summary

You now have a basic structure for your Haskell project in the `web/` folder, with a `MazeLogic.hs` module that handles maze generation and pathfinding, and a `WebMain.hs` module that exports these functions for use in JavaScript. You can expand upon the maze generation and pathfinding logic as needed.
module Hurtle.Types where

import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss (Picture)

--------------------------------------------------------------------------------
-- Type Definitions


-- | A Hogo program is a list of HogoCode instructions.
type HogoProgram = [HogoCode]


data HogoCode
  -- | Movement Commands
  = GoForward Float
  | GoBackward Float
  | TurnLeft Float
  | TurnRight Float
  | GoHome
  -- | Pen Commands
  | PenUp
  | PenDown
  | ClearScreen
  -- | Control Flow
  | Repeat Int HogoProgram
  deriving (Show,Read,Eq)


-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String


-- | Data to represent turtle which uses hogo commands to illustrate 
data TurtleInk = TurtleInk 
  { position :: (Float, Float) -- coordinate of turtle on the board
  , angle :: Float -- angle turtle is pointing to
  , pendown :: Bool -- is turtle currently drawing
  , linesDrawnSoFar :: [((Float, Float), (Float, Float))] -- all ink turtle has laid
  } deriving (Show)


-- | represents each cell in a maze, coordinates corresponding to the 2D list
type Cell = (Int, Int)
-- | 2D List structure which represents the maze, false = wall, true = path
type Maze = [[Bool]] 


-- | Represents the state of the turtle in the maze, and the maze itself
data TurtleState = TurtleState 
  { coord :: (Int, Int) -- coordinate of turtle on the maze
  , turtlePath :: [(Cell, Cell)] -- drawing of turtles path in maze
  , maze :: Maze -- 2D list representing maze turtle is in
  , mazeSize :: Int -- size of maze turtle is in
  , mazePic :: Picture -- picture of maze turtle is in
  , endPoint :: (Int, Int) -- end coordinates of maze turtle is in
  , perfectRun :: [Cell] -- Coords for perfect run
  , scaleSize :: Float -- the value used to scale images in maze correctly
  , startLevel :: Int -- the level user decided to start at
  } deriving (Show)


-- | Representing a node in a maze, used for aStar algorithm
data Node = Node 
    { currentPos :: (Int, Int) -- current position in maze
    , gCost :: Int -- cost to move from start to current point
    , fCost :: Int -- estimated cost from current point to end (using manhattan distance)
    , parent :: Maybe Node -- link to original node
    } deriving (Eq, Show)
    


{- Loading generic png, in types as used by viewer and maze -}
loadImage :: FilePath -> IO Picture 
loadImage filePath = do 
    maybeImage <- loadJuicyPNG filePath
    case maybeImage of 
        Nothing -> error $ "Could not load:" ++ filePath
        Just pic -> pure pic
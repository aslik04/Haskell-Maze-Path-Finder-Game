module Hurtle.MazeGame where

import Hurtle.Types (TurtleState(coord, turtlePath, maze, mazeSize, mazePic, endPoint, perfectRun, scaleSize, startLevel, TurtleState )
                    , Cell
                    , loadImage)
import Hurtle.Lines (askForColor, intLinesToPicture)
import Hurtle.PrimMazeGenerator (generateMaze)
import Hurtle.RenderMaze (renderMaze)
import Graphics.Gloss.Interface.IO.Game
    ( Picture(Scale, Blank, Pictures, Color, Line, Translate, Rotate),
      red,
      white,
      scale,
      translate,
      Display(InWindow),
      Color,
      playIO,
      Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeySpace, KeyUp, KeyDown, KeyLeft, KeyRight),
      Event(EventKey) )
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.List (nub)


{- This is the main function of the maze game, which displays the entirety of the game to the user, 
   utilising do notation for readability and to easily unwrap elements from their lifted states. 
   The terminal will ask a user for a number to form a square maze, if the user fails to give a number, 
   the process will loop until given, if the user does give a number the game will process. Where the user
   is able to choose a colour which will be used to draw the lines for the turtles path in the maze. 
   a maze image is generated through various processes, and is stored in the turtles state, this is done for 
   constant access to maze data, which allows the game to restart the maze whenever a space bar is pressed. 
   The maze image is rendered with the sand background and grass shrubs as the walls, and the turtle can not
   move into walls only paths. The goal of the game is to reach the end in the shortest number of steps possible, 
   in order to progress to the next level the player must solve the maze optimally, or user loses and goes back to initial level. 
   this is evaluated by comparing to the number of steps aStar took to solve the maze. Turtle is controlled by 
   arrow keys, and game is restarted using space bar. Optimal path displayed after game ends as a red lines. -}
gameToWorld :: IO ()
gameToWorld = do
    -- Asks player what size of square maze they would like
    putStrLn "Enter the Level you would like to start at (e.g. 10 is a 10x10 maze):"
    -- Ensure line is printed
    hFlush stdout
    -- retrieve the input from the user
    input <- getLine
    -- convert input string into a maybe int using read
    case readMaybe input :: Maybe Int of
        -- If string input wasnt a number print an error and loop game until correct input given
        Nothing  -> do
            putStrLn "Please enter a valid number; larger mazes will take longer to render."
            -- start process again
            gameToWorld
        -- correct input given so now can play game
        Just mazeInt -> do
            -- Call secondary function defined in Lines, to ask user for color turtle will draw
            colour <- askForColor
            -- generates a tuple of a 2D maze list, and the list of coordinates to solve maze optimally
            (mazeList, optimalRoute) <- generateMaze mazeInt
            -- using the mazeList render an image of the maze, and is scaleable size of each cell as a tuple
            (mazeImage, cellSize) <- renderMaze mazeList mazeInt
            -- initialise the offset of the maze, in order to translate image to correct positions
            let offset = findOffset (fromIntegral mazeInt)
                -- centre the image of the maze using offset, so it aligns with the background
                centreMaze = Translate offset offset mazeImage
            -- initialise the beginning state of the turtle, which starts at beginning and has no paths drawn
            let initialState = TurtleState {
                coord = (1, 1) -- coord of start, as walls surround, maze starts at (1, 1)
                , turtlePath = [] -- empty list as turtle has not drawn any paths yet
                , maze = mazeList -- the 2D list of the maze the turtle is currently in
                , mazeSize = mazeInt -- the size of the maze desired by the user
                , mazePic = centreMaze -- the image of the maze correctly centred, drawn by drawGame
                , endPoint = (mazeInt, mazeInt) -- tuple of the coordinates of the goal
                , perfectRun = optimalRoute -- list of coordinates which form the optimal route
                , scaleSize = cellSize -- used to scale the images correctly
                , startLevel = mazeInt -- level user decides to start at
                }
            playIO (InWindow "Hurtle Maze Explorer" (500, 500) (0, 0)) white 30 initialState -- 500 pixel window
                (drawGame colour) -- draw the turtle its path, and the maze, 
                                                    -- allows restarting mazePic due to being stored in turtlestate
                pressKey -- read inputs from user in form of events as key pressed on key board
                updateGame -- not used in this game, maybe could be implemented in more advanced iterations


{- Function not utilised, however needed for PlayIO -}
updateGame :: Float -> TurtleState -> IO TurtleState
updateGame _ = pure


{- Key method in order to display all the correct images within the window and in the order they should be displayed. 
   Utilises do notation in order to unwrap functions in a readable manner, and clearly display how everything fits together. 
   Method uses colour decided by user to draw the a lined path of the turtle as it traverses through the maze. Offset and 
   cellSize are neccessary for the scaling and translating of the images toward the correct graphical positions. State contains
   the key maze data, this is designed this way as it allows me to restart the maze by using data stored within the turtle. 
   Alist of images which correspond to the order they are imposed is returned, with gameOver being last to be imposed ontop. -}
drawGame :: Color       -- The colour which the user decided, which is read from input line, used to draw turtles path
         -> TurtleState -- state of turtle containing current position, and maze images and data, and optimal path
         -> IO Picture  -- Returns a lifted set of pictures in order they should be imposed over eachother
drawGame colour state = do
    let cellSize = scaleSize state
    let offset = findOffset (fromIntegral (mazeSize state))
    -- Game over only drawn if the turtle is at the end point in maze
    gameOver <- if coord state == endPoint state
        -- chooses correct ending for how the player performed
        then chooseEnding state
        -- if turtle is not at the end point, turtle can continue and game over not presented
        else pure Blank
    -- loads background image which is a sand background
    backImage <- loadImage "assets/sand.png"
    -- loads image of the turtle defined in assets
    turtleImage <- loadImage "assets/turtle.png"
    -- retrieves correct image of maze, which is stored in the turtle state
    let mazeImage = mazePic state
    -- If the turtle has reach the end point display the optimal path in red
    optimal <- if coord state == endPoint state
        -- calls optimalDisplay helper which scales and translates red optimal path correctly
        then pure (optimalDisplay (perfectRun state) offset cellSize)
        -- if turtle hasnt reached end yet, do not show the optimal path
        else pure Blank
    -- the turtles path as it moves is drawn using colour requested, using helper function from Lines.hs
    let path = intLinesToPicture colour (turtlePath state)
        -- drawn path is scaled and translated to correct position using cellSize
        centrePath = Translate offset offset $ Rotate 180 $ Scale (-cellSize) cellSize path
    -- current coordinate of the turtle in the maze
    let (x, y) = coord state
        -- generates a graphical x coordinate for the turtle correctly placing it within a cell
        actualX = ((fromIntegral x - 1) * cellSize) + offset + cellSize
        -- generates a graphical y coordinate of the turtle to be placed inside correct cell
        actualY = ((fromIntegral y - 1) * cellSize) + offset + cellSize
        -- turtle is correctly scaled and translated using calculated coordinates to be within corrct cell
        turtle = Translate actualX actualY $ Scale (cellSize / 500) (cellSize / 500) turtleImage
    -- returns lifted list of all the images in the order they should be displayed over each other
    pure $ Pictures [backImage, mazeImage, centrePath, turtle, optimal, gameOver]


{- Function used to detect events, in case of Hurtle Maze, only the arrow keys and space initiate actions, 
   others will just return the original state. Utilised nested Case of, for readability and concise 
   decleration of each event neccessary to the game. For the arrow keys, utilises drawMove helper function, 
   to decide if the corresponding translation of coordinates doesnt contain a wall, and if it doesnt, the current 
   coord of turtle is modified and also creates a new path in (turtlePath) which allows the new path to be drawn
   in drawGame. If space bar is pressed, we want to restart the maze, in order to do this while the gameIO is playing, 
   i needed to store the maze data within the turtleState, so this helper function will modify the maze image stored, 
   will modify the 2D maze list, and the optimal path it contains, all which will be used to display a new maze in the 
   drawGame function. Where notation used to concisely display where x and y coordinates come from -}
pressKey :: Event          -- Key board event, which if correct key will initiate an action
         -> TurtleState    -- current state of turtle in the maze, containing data about itself and maze
         -> IO TurtleState -- Lifted turtle state, after correct actions have been applied to state
pressKey event state =
    if endPoint state /= coord state then -- as long as the turtle isnt at the end, allow keys to move turtle
        case event of
        -- SpecialKey from IO.Game, utilised to detect key actions
        EventKey (SpecialKey button) Down _ _ ->
            -- Nested case of to detect which key has been pressed
            case button of
                -- If up arrow key has been pressed move the turtle up the maze, by moving its y coordinate up
                KeyUp    -> pure $ drawMove state (x, y + 1)
                -- If down arrow key has been pressed move the turtle down the maze, by moving its y coordinate down
                KeyDown  -> pure $ drawMove state (x, y - 1)
                -- If left arrow key has been pressed move the turtle left the maze, by moving its x coordinate left
                KeyLeft  -> pure $ drawMove state (x - 1, y)
                -- If right arrow key has been pressed move the turtle right the maze, by moving its x coordinate right
                KeyRight -> pure $ drawMove state (x + 1, y)
                -- If space bar is pressed, call helper function 
                -- which correctly modify maze data stored in turtle which will generate new maze
                KeySpace -> restartMaze state
                -- If any other key is pressed return original state lifted, and this will provide no movement in actual game
                _        -> pure state
        -- Any other events are also ignored, and only the keys listed above.
        _ -> pure state
    else case event of -- if turtle is at the end only space bar will cause a command
        -- If space bar is pressed, call helper function 
        -- which correctly modify maze data stored in turtle which will generate new maze
        EventKey (SpecialKey KeySpace) Down _ _ -> restartMaze state
        _ -> pure state
    where
        -- Decleration of turtles current coordinate in maze
        (x, y) = coord state


{- This is a helper function which utilising do notation for readability, and loads in two seperate
   images for win and lose, if the steps taken by turtle is same as optimal then player wins. 
   Steps are the length of optimal -1, due to it containing goal coord and turtle never will. 
   Next as turtle path contains a pair of tuples which connect points, we only need first element
   of each,and we use nub to remove any duplicate elements, and this will represent the turtles coords. 
   If they are the same length player has found shortest oath, else they have not and have lost game -}
chooseEnding :: TurtleState -- state which contains data on steps made and optimal steps
             -> IO Picture  -- Picture corresponding to whether they won or not
chooseEnding state = do
    levelImage <- loadLevelImage (mazeSize state)
    -- Loading image of winning game over, press space to restart game
    gameWin <- loadImage "assets/LevelComplete.png"
    -- Loading image of losing game over, press space to restart game
    gameLost <- loadImage "assets/gameLost.png"
    if winOrLose state
        -- Display lifted scaled gameWin image
        then pure $ scale 0.3 0.3 (Pictures (gameWin : centeredLevel levelImage))
        -- If steps are not the same player loses
        else pure $ scale 0.2 0.2 gameLost


{- Helper function which uses let in notation for enhanced readability, 
   Function is used to check if the user performed the optimal path in the maze, 
   and therefore has won the game, this is done by first removing duplicate coordinates 
   in turtlePath, duplicate coordinates occur when the player tries to move into a wall. 
   They are removed using nub from Data.List, utilisng map to only get the first tuple from
   every nested tuple within turtlePath, this is done as turtlePath is formed when
   coordinates are constantly connected by eachother forming a vector. Finally after this 
   if the lengths are the same, the user has achieved the path in the optimal number of steps. -}
winOrLose :: TurtleState -- Current state of turtle containing optimal path and path it used
          -> Bool        -- True if turtle performed optimally, False if greater steps used
winOrLose state =
    -- nub removes duplicates, and we retrieve every first pair in the turtlePath tuple
    let pathByTurtle = nub $ map fst (turtlePath state)
    {- If same number of steps made then perfect, we remove 1 as perfect contains goal coord, 
       and pathByTurtle never will as it ends there -}
    in length pathByTurtle == length (perfectRun state) - 1


{- Helper function designed to modify the maze data stored within a turtle, so when the state of turtle is used within
   drawGame, this uses newly generated maze data. Do notation used for readability over combinators. First we generate new
   maze depending on the size the user originally asked for, and generates ooptimal route. using this we create a new image, 
   and centre the image. All of this new data is used to modify existing data in the turtle state, and we lift the state returning it -}
restartMaze :: TurtleState    -- Current state of turtle, containing current Maze picture, Maze list, and optimal path
            -> IO TurtleState -- Lifted state of turtle, after new Maze image declared, new Maze list added, and new optimal route
restartMaze state = do
    let size = if winOrLose state
        -- If user won, level progresses and maze increases by one
        then mazeSize state + 1
        -- if user lost and couldnt solve maze optimally, reset maze back to initial size when user inputed
        else startLevel state
    -- generates a new maze List corresponding to original size asked by user, also gets coordinates of optimal path
    (mazeList, optimalRoute) <- generateMaze size
    -- Using 2D maze list, the coordinates are mapped into an image of the maze using renderMaze from (RenderMaze.hs)
    (mazeImage, cellSize) <- renderMaze mazeList size
    -- calculates how much maze image should be offset by to align with the background image
    let offset = findOffset (fromIntegral size)
        -- Using calculated offset, aligns maze image with the background
        centreMaze = Translate offset offset mazeImage
    {- Modifies the state to contain new Maze data which will be used by drawGame to draw a new maze, 
       all other data is reset to basic, starting coord and turtlePath which will wipe the path of the turtle draw to empty -}
    pure state { coord = (1, 1) -- start turtle at beginning position
               , turtlePath = [] -- wipe the drawn path back to empty
               , mazePic = centreMaze -- modifiy image stored in turtle to new generated image
               , endPoint = (size, size) -- the coordinates of the goal
               , mazeSize = size -- the size of the maze after reset
               , maze = mazeList -- 2D list stored in turtle modified to new list
               , perfectRun = optimalRoute --  new optimal route based on new maze
               , scaleSize = cellSize -- used to scale images correctly
               }


{- This function utilises let in notation for readability where it declares the use
   of the helper functions it utilises before returning how it modifies turtle state. 
   Function will only generate generate new move into trutle state if the new move doesnt contain
   a wall. If the position does have a wall, the turtles current coord is modified to new pos, 
   and then the turtlePath in appended to store coordinates of the new move as a tuple. -}
drawMove :: TurtleState -- Current state of turtle containing data on position
         -> Cell        -- The coordinate we want to draw turtle to
         -> TurtleState -- Turtle if new coord was successful, and has no wall
drawMove state desiredCoord =
    -- Helper function (move) checks if a wall exists in desired position, if it does return old coord
    let newCoord = move state desiredCoord
        -- Constructs a new path depending on the result of coord
        newPath = (coord state, newCoord)
    -- returns modified state which changes current position of turtle to new, and modifies its path
    in state { coord = newCoord
             , turtlePath = turtlePath state ++ [newPath] }


{- Helper function designed for modularity and to readibly allow the analysis of a new coordinate, 
   and decide if it should be moved to depending on if a wall exists. -}
move :: TurtleState -- state of turtle in maze, needed for helper function (notWall)
     -> Cell         -- The coordinate we want to movd turtle to
     -> Cell        -- Returns the correct movement based on the angle given
move state (newX, newY) =
    -- whether move should be valid is all dependent on if the coord move to doesnt have a wall
    if notWall (newX, newY) state
        -- True means no wall, and can return desired position
        then (newX, newY)
        -- else return the old position back out
        else coord state


{- Simple helper function which directly accesses, the 2D list maze 
   accesses which list using x coodinate of, and which element in that
   list using the x coordinate and then returns the value True or False. 
   With True meaning empty path, and False meaning there is a Wall -}
notWall :: Cell        -- The coordinates in maze we are checking for wall
            -> TurtleState -- The state of the turtle which stores the maze it is in
            -> Bool        -- True if there is no wall, False if there is a wall
notWall (x, y) state =
    -- Using (!!) operator which returns corresponding element in list, used twice for 2D list
    maze state !! y !! x


{- Utilised let in notation, due to readability, allowing declaring over helper function before, 
   and declaring the result finally. The arithmetic for finding where the inWindow coordinate belongs
   is similar, using the (x or y) coordinate multiplied by the scale(cellSize) this translates the coord
   from centre, to the beginning of the cell itt belongs to, then we (+ cellSize / 2) to bring coord to 
   centre of the cell, where the correct path can be drawn. -}
cellToPath :: Float          -- Used to find in window graphical coordinate, using maze coordinate
           -> Cell           -- Coordinate of the vector being mapped into window
           -> (Float, Float) -- Tuple containing the direct inWindow coordinate of the vector
cellToPath cellSize (x, y) =
    -- Arithmetic to find graphical x coordinate
    let pathX = (fromIntegral x * cellSize + cellSize / 2)
        -- arithmetic to find graphical y coordinate
        pathY = -(fromIntegral y * cellSize + cellSize / 2)
    in (pathX, pathY)


{- Helper function which converts a list of coordinates, which connect to make an optimal path, these are
   drawn using red and Line from Gloss libraries, and each element in list of coords is aplied to cellToPath, 
   by using the map function, the coordinates are then correctly mapped into an image and centred. -}
drawOptimal :: [Cell]  -- The list of coordinates which make up the optimal path
            -> Float   -- cellSize scale, which allows a maze coord to be brought to the correct inWindow position
            -> Picture -- Picture containing the optimal path drawn together, in red
drawOptimal path cellSize =
    -- red line which is maped over by helper (cellToPath) function using each element in the list of coords
    Color red $ Line $ map (cellToPath cellSize) path


{- Third function pertaining to the drawing of the optimal path, this was done for modularity and readability, 
   Where notation used to limit crowding of code, and readibly declare how much image should be translated by. 
   Method utilises helper functions above, to first draw the timage of the optimal path, then mirror flip it, 
   and rotate 180 degrees, and translate to correct position in maze, in order for it be correctly represented -}
optimalDisplay :: [Cell]  -- List of coordinates which make up the optimal path. 
               -> Float   -- The amount the image needs to be offset by in order to fit in the overal picture
               -> Float   -- The cellSize scale used to calculate graphical coords from maze coords 
               -> Picture -- Image of optimal path translated to correct graphical position
optimalDisplay path offset cellSize =
    -- Utilises translate and rotate from gloss, and helper function above
    Translate cent cent $ Rotate 180 $ Scale (-1) 1 $ drawOptimal path cellSize
        where
            -- How much the image should be offset by.
            cent = offset - (cellSize / 2)


{- Function to centre digits in the levels number Image, to correct position to align across 
  Level _ Completed Image, and this uses where notation for readbility allowing the main fuction
  to appear more concisely. zipWith is used to pair each picture with its coordinate so that
  each digit can be correctly spaced, and it doesnt matter how many digits are in the number, 
  they will always be correctly spaced.  -}
centeredLevel :: [Picture] -- Picture of the level number without being translated to correct pos
              -> [Picture] -- Picture after being translated to correct position
centeredLevel levels =
    {- zipWith utilised to combine each digit image with its index in the list of pictures, 
    each index and level, in the newly formed tuple from zipWith using lambda, is then used 
    in order to offset the image correctly into right positon. -}
    zipWith (\x level -> translate (vert x) 25 level) [0..] levels
    where
        -- the value of the vertical offset
        vert :: Int -> Float
        vert x = fromIntegral x * 100 + 150


{- Function used to generate a generic number of file paths from the digits 0-9, these 
   are collectively used to form the image of the level to be displayed after completing a level, 
   show is used to to turn the level into a string, and then map uses each digit in the newly string, 
   to form a file path which is then combined into a list of file paths in correct order. -}
levelFilePath :: Show a => a -- All types which have a show instance can be used as level
              -> [FilePath]  -- Order list of file path in same order digits of level are
levelFilePath level =
    map (\num -> "assets/Numbers/" ++ [num] ++ ".png") (show level)


{- Function used to turn a level number, first into a list of file paths using helper function, 
   levelFilePath, and then these filePaths are loaded one by one and lifted, into a list of
   lifted Pictures, using mapM -}
loadLevelImage :: Show a => a  -- All types which have a show instance can be used as level
               -> IO [Picture] -- Lifted set of pictures for each digit in the level
loadLevelImage level =
    {- mapM to lift each digit in the level into an image using loadImage then form a list -}
    mapM loadImage (levelFilePath level)


{- Helper function, which takes an input of how large user asked maze to be, 
   returns corresponding offset, this was done as with different size of maze, 
   there was a margin of error within the estimated offset of (-250) which is
   half the size of the background image, done as the maze intialises from 0, 
   however as we are dealing with floats, change in size meant slightly different offsets -}
findOffset :: Float -- Size of the maze, which needs to be offset
           -> Float -- The float of how much the image needs to be translated
findOffset x
    | x == 1 = -170
    | x == 2 = -190
    | x == 3 = -200
    | x == 4 = -210
    | x == 5 = -215
    | x < 8  = -220
    | x < 11 = -230
    | x < 21 = -238
    | x < 31 = -242
    | x < 41 = -244
    | x < 51 = -245
    | x < 61 = -246
    | otherwise = -247
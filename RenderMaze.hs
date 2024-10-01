module Hurtle.RenderMaze where

import Hurtle.Types (Maze, loadImage)
import Graphics.Gloss ( Picture(Blank, Scale, Color, Pictures, Translate)
                      , red
                      , rectangleSolid )


{- Utilising do notation for its favourable readability, The wall image is loaded in, 
   which was predefined in Huurtle.Types. The graphical scale for each cell is calculated, 
   by dividing the size of the background image which is 500 by the number of cells in a 
   a maze row + 2 for the walls on either side, this calculation allows us to scale the wall
   image to the correct graphical coordinate in the window corresponding to its location 
   in the 2D maze List. Once the wall is scaled, the walls are mapped over each cell in a row
   and each row in the maze where a value of False in a call will map a wall to that
   graphical coordinate, and concatenated to form a list of images forming combined maze. 
   A red square corresponding to the end of the maze is initialised to help the user, 
   know where the goal of the maze game is. Finally the list of images and the calculated
   cellSize scale is lifted and returned. -}
renderMaze :: Maze                -- The 2D list we are converting into an image
           -> Int                 -- Size of the maze we are rendering 
           -> IO (Picture, Float) -- Lifted Picture of maze and size of maze
renderMaze maze mazeSize = do
    -- Loading the png of the wall image
    wallImage <- loadImage "assets/Wall.png"
    {- Background is 500 x 500 pixels, size of cell image calculated by dividing 
       by number of cells (+2 for the walls on either side) in one row of maze -}
    let cellSize = 500 / (fromIntegral mazeSize + 2)
        -- The size wall image should be scaled down to
        wallScale = cellSize / 500
        -- Each wall in maze scaled down to fit one cell.
        scaledWall = Scale wallScale wallScale wallImage
    {- First renderRow is mapped over each list in the maze parameter of type [[Bool]], 
       zip is used to pair each list with its correct y coordinate, starting from 0, 
       Once each row has been rendered into an Image the images are concatenated into
       a list of images which represent the complete maze Picture -}
    let mazeImage = Pictures $ concatMap (renderRow scaledWall cellSize) (zip [0..] maze)
    -- The end position of cell calculated by getting top inner corner of maze
    let endPos = fromIntegral mazeSize * cellSize
        -- Generic red square of size cellsize
        redSquare = Color red $ rectangleSolid cellSize cellSize
        -- Translates red square to top corner of the maze
        endSquare = Translate endPos endPos redSquare
    -- Returns lifted functions which is the combined maze image and size of each cell image
    pure (Pictures [mazeImage, endSquare], cellSize)


{- An entire row of cells in a maze, is rendered where wall image is correctly placed in
   corresponding coordinate in grid by multiplying coordinate by cellSize, which allows 
   the correct graphical representation of wall. The list of bools in tuple correspond to
   the list of cells in a row, and False means a wall will be placed in that graphical position -}
renderRow :: Picture       -- Image which represents a wall in the Maze
          -> Float         -- Size of cell in Window, used to scale to correct positions 
          -> (Int, [Bool]) {- Tuple where y is the y coordinate of row within the maze, 
                              and list of booleans corresponding to cells in row -}
          -> [Picture]     -- Picture representing each wall in the row
renderRow wallImage cellSize (y, row) =
   {- zipWith utilised to apply the bracketed function to each Cell in the row, and 
      its corresponding index in the row because of [0..] which is infinite, and therefore 
      zipWith will end when all cells in row are processed. renderCell function utilised
      to form a translated image of where the wall should be in the maze image. -}
   zipWith (curry (renderCell wallImage cellSize y)) [0..] row


{- Renders single cell in maze, to be displayed in window, where cellSize is the number 
   of pixels one cell represents in the window. Walls are portrayed when Cells value is 
   False, and displays wallImage, else if true displays Blank for empty path. 
   Translate used to move wall Image to correct graphical representation -}
renderCell :: Picture     -- Image of wall to correct scale
           -> Float       -- Size of cell in Window, used to scale to correct positions  
           -> Int         -- y coordinate of cell within grid, used to calculate vertical pos
           -> (Int, Bool) -- Tuple of x coordinate, and Bool, where False is Wall and True is not
           -> Picture     -- Complete representation of cell, translated to correct place
renderCell wallImage cellSize y (x, cell)
    -- if cell is False, translate wall to correct coordinate depending on x and y
    | not cell  = Translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) wallImage
    -- if True, then leave blank to simulate an empty path
    | otherwise = Blank
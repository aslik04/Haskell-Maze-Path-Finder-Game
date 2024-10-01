module Hurtle.Viewer where 

import Hurtle.Types ( TurtleInk (position, angle, pendown, linesDrawnSoFar, TurtleInk)
                    , HogoCode (GoForward, GoBackward, TurnLeft, TurnRight, GoHome, PenUp, PenDown, ClearScreen, Repeat))
import Prelude hiding (mod)
import Data.Fixed (mod')
import Graphics.Gloss ( white, display, Display(InWindow) )
import Hurtle.Lines (linesToPicture, askForColor)
import Hurtle.Parser (parseHogo)
import Text.Megaparsec ( parse, errorBundlePretty )


{- Helper function describing initial state of the turtle used for rendering, 
   or resetting the turtle, implemented to reduce repetitive code -}
initialTurtleInk :: TurtleInk 
initialTurtleInk = TurtleInk (0, 0) 0 True [] 


{- Helper function to convert degrees into radians for added precison, 
   and is needed to use cos and sin from prelude, as they expect radians. -}
toRadians :: Float -- The degree of which turtle is turning left or right 
          -> Float -- Radians of degree, which now can be used by sin and cos
toRadians degree = (pi / 180) * degree 


{- Helper function which calculates new position based on current position, 
   angle turtle is facing and the distance required to move. 
   Utilisation of trignometry to calculate x' and y' 
   which represent change in x and y coordinates respectively -}
newPosition :: (Float, Float) -- coordinates of current position
            -> Float          -- The angle want to turn turtle to
            -> Float          -- The distance want to move turtle 
            -> (Float, Float) -- The new calculated coordinates of turtle
newPosition (x, y) radians distance = (x + x', y + y')
    where 
        x' = cos radians * distance 
        y' = sin radians * distance


{- Method to update the state of the turtle, to update where the turtle should ink, 
   calculates newPosition using helper function (newPosition) and (toRadians) to
   convert angle stored in state to radians to use prelude trignometry functions
   I utilised where due to its readability, clearly displaying where the
   updated position and lines is calculated to form the new state. 
   Furthermore, i utilised list comprehnsion due to its simple 
   and readable nature as it indicates clearly the appending only happens
   if the pendown is True else will return the linesDrawnSoFar.  -}
turtleMove :: TurtleInk -- Data used to represent turtles current ink status
           -> Float     -- The distance want to move the turtle
           -> TurtleInk -- The new state of turtle after turtle has moved
turtleMove turtle distance = turtle { position = newCoord, linesDrawnSoFar = ink }
    where 
        -- Current position of turtle
        (x, y) = position turtle 
        -- trigonometrically calculated new position
        newCoord = newPosition (x, y) (toRadians (angle turtle)) distance
        -- list comprehension to only append if pendown == true
        ink = linesDrawnSoFar turtle ++ [((x, y), newCoord) | pendown turtle]


{- I decided to pattern match for this function as i believe it is a idiomatic
   readable display of each hogo command and how it is put into action, 
   initially using case of however, i felt like it crowded the readability  -}
updateTurtleInk :: TurtleInk -- current state of turtle in the window
                -> HogoCode  -- the command from user which needs to move turtle
                -> TurtleInk -- the updated state after command has been applied
{- Moves the turtle forward corresponding to distance specified, 
   by utilising helper function turtleMove which updates state of turtle -}
updateTurtleInk turtle (GoForward distance) = 
    turtleMove turtle distance
{- Moves the turtle backwards corresponding to negated distance specified, 
   by utilising helper function turtleMove which updates state of turtle -}
updateTurtleInk turtle (GoBackward distance) = 
    turtleMove turtle (-distance)
{- Turns the turtle counter clockwise to correspond to turning left, 
   This is achieved through adding degree parameter to the turtles current angle, 
   however by using the modulo operator we return the turtle back within 360 degrees, 
   and correctly imulate the turning of the turtle -}
updateTurtleInk turtle (TurnLeft degree) = 
    turtle { angle = (angle turtle + degree) `mod'` 360 }
{- Turns the turtle clockwise to correspond to turning right, Technique described above -}
updateTurtleInk turtle (TurnRight degree) = 
    turtle { angle = (angle turtle - degree) `mod'` 360 }
{- Sets pendown to False, so when turtleMove is now called subsequent lines, 
   will not be appended to the linesDrawnSoFar list and thus wont be drawn -}
updateTurtleInk turtle PenUp = 
    turtle { pendown = False }
{- Sets pendown to True, so when turtleMove is now called subsequent lines, 
   will be appended to the linesDrawnSoFar list and thus will be drawn -}
updateTurtleInk turtle PenDown = 
    turtle { pendown = True }
{- Resets the turtles state back to what it was originally thus, 
   removing any lines drawn and returning back to origin, with pendown. -}
updateTurtleInk _ ClearScreen = initialTurtleInk
{- Updates state of turtle to send turtle back to the origin  -}
updateTurtleInk turtle GoHome = 
    turtle { position = (0, 0) }
{- Updating the state of turtle when repeat command is called, which repeats hogocode, 
   (foldl) utilised for its accumulating nature, which applies the elements of the list, 
   until the list is exhausted, elements are applied to (updateTurtleInk) in a 
   successive manner, this ensures that the correct drawing is formed from the 
   repetitions, as the state of the turtle is updated successively.
   (concatMap) utilised due to its ability to map (replicate num) to each 
   part of (hogoCode) and then the code is concatenated together to form a list.
   This list is what is used by foldl to successively fold over -}
updateTurtleInk turtle (Repeat num hogoCode) = 
    foldl updateTurtleInk turtle $ concatMap (replicate num) hogoCode


{- I used do notation due to its enhanced readability over combinators, 
   the image of the turtle is loaded in, and the color is requested from the user. 
   In order to retrieve the image from the HogoCode we fold over the hogocode, 
   and append to the linesDrawnSoFar by utilising the (updateTurtleInk) method, 
   foldl utilised due to its accumulative nature which combines the result  
   successively which allows the hogocode to be used one after another in succession.
   The turtle image is translated to fit the screen and then displayed in the window
   with a white background and the image it has drawn. -}
displayHogo :: FilePath -- file from user containing all the hogo code to ink umage
            -> IO ()    -- IO to display to real world, called in main
displayHogo filePath = do 
   file <- readFile filePath
   case parse parseHogo filePath file of 
      Left bundle -> putStrLn $ errorBundlePretty bundle 
      Right program -> do
         -- recursively ask user for color until valid one given
         colour <- askForColor
         -- accumulatively fold over state of turtle and its hogo code usng updateTurtleInk
         let state = foldl updateTurtleInk initialTurtleInk program
         -- generate an image of all the lines drawn by turtle
         let linesImage = linesToPicture colour (linesDrawnSoFar state)
         -- display the combined pictures into one image window size (500, 500) 
         -- beginning at (0, 0) with a white background 
         display (InWindow "Hurtle Viewer" (500, 500) (0, 0) ) white linesImage
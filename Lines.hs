module Hurtle.Lines where 

import Text.Megaparsec (parse)
import System.IO (hFlush, stdout)
import Graphics.Gloss ( Picture(Line, Pictures, Color), Color ) 
import Hurtle.Parser (parseTurtleColor)


{- Helper function which asks user to select a colour from the list below, 
   input is attempted to be parsed into type (Color) using (parseTurtleColor)
   defined in Parser.hs, if the parsing fails and is not from that list, 
   user is asked again, and error message is printed out, this is repeated 
   until a valid colour is parsed and is then returned. -}
askForColor :: IO Color
askForColor = do 
    -- requests color from user from list
    putStrLn "Enter Color (black, green, yellow, blue, cyan, magenta):"
    -- ensures that the prompt is immediately displayed
    hFlush stdout
    -- retrieves the input given from users
    colorInput <- getLine
    -- case of the parsing result of helper function
    case parse parseTurtleColor "" colorInput of 
        -- if a valid color not returned then loop until given, 
        -- and inform user of the only valid color options 
        Left _ -> do 
            putStrLn "Pick from: black, green, yellow, blue, cyan, magenta"
            askForColor
        -- return the valid colour, using pure which lists colour onto IO 
        Right colour -> pure colour 


{- Converts a list of vectors, into a complete Line image, each vector is represented 
   by a pair of coordinates, a picture of the combined lines is generated from the color
   that the user specifies. Utilises Picture constructor from Gloss in order to 
   combine all the images of the lines into one single image in specified color -}
linesToPicture :: Color -> [((Float, Float), (Float, Float))] -> Picture
linesToPicture colour vectors = 
    Pictures [ generateLine colour vector | vector <- vectors ]


{- Converts a list of vectors, into a complete Line image, each vector is represented 
   by a pair of coordinates, a picture of the combined lines is generated from the color
   that the user specifies. Utilises Picture constructor from Gloss in order to 
   combine all the images of the lines into one single image in specified color -}
intLinesToPicture :: Color -> [((Int, Int), (Int, Int))] -> Picture
intLinesToPicture colour vectors = 
    Pictures [ generateLine colour (intToFloat vector) | vector <- vectors ]
    where 
        -- helper function to convert int coordinates into floats which can be used by gloss
        intToFloat ((x1, y1), (x2, y2)) = 
            ((fromIntegral x1, fromIntegral y1), (fromIntegral x2, fromIntegral y2))


{- Generates a picture of a line, from a vector and its two coordinates, start and end, 
   Color function from Gloss utilised to set the color of the line to that specified
   by the user. As Gloss' coordinate system starts from the centre, and the y axis 
   extending up, we negate the y axes to follow a traditional coordinate system -}
generateLine :: Color -> ((Float, Float), (Float, Float)) -> Picture
generateLine colour ((x1, y1), (x2, y2)) = Color colour $ Line [(x1, -y1), (x2, -y2)] 
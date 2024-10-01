module Hurtle.Parser where

import Hurtle.Types (HogoCode (GoForward, GoBackward, TurnLeft, TurnRight, GoHome, PenUp, PenDown, ClearScreen, Repeat)
                    , Parser
                    , HogoProgram)
-- You'll probably want to refer to https://hackage.haskell.org/package/megaparsec for documentation of the Megaparsec library.
import Text.Megaparsec (try, choice, some, many, between, eof, (<|>))
import Text.Megaparsec.Char (string, digitChar, newline, space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Control.Monad (void, when)
import Graphics.Gloss ( black, blue, cyan, green, magenta, yellow, Color )


{- Do notation utilised to improve readability over use of combinators, 
   which in my opinion can become very clustered for the reader of my code. 
   Where _ <-, is used it means that the result can be ignored, 
   this is done to ignore any whitespace/comments or to skip till end of file
   using (many) allows the function parseCommand to be applied multiple times
   to the file of HogoCode, and pure allos the HogoCode to be returned once parsed -}
parseHogo :: Parser HogoProgram
parseHogo = do
    _ <- skipSpace -- ignore whitespace and comments
    hogoCommands <- many parseCommand -- apply parseCommand many times
    _ <- eof -- once hogo commands consumed skip to end of file
    pure hogoCommands -- return HogoCode from file which can then be applied


{- Parses WhiteSpace, Single Comments(;) and Block Comments (-; ;-)
   Allows the use of comments and spaces in Hogo File -}
skipSpace :: Parser ()
skipSpace = Lexer.space -- Takes 3 Parsers, a base case, line, and block parser
    space1 -- Skips WhiteSpace including new lines (from Text.Megaparsec.Char)
    (Lexer.skipLineComment ";") {- Single Line comments, all character on the 
                                same line after (;) is ignored by parser
                                from (Text.Megaparsec.Char.Lexer) -}
    (Lexer.skipBlockComment "-;" ";-") {- Skips block comments, all characters, 
                                between ";*" and "*;" are ignored by parser, 
                                from (Text.Megaparsec.Char.Lexer) -}


{- Takes String as argument, and if sucessfully parsed, 
   then uses skipSpace defined above to ignore 
   any whitespace or comments following the Parser -}
symbol :: String -> Parser String
symbol = Lexer.symbol skipSpace -- from (Text.Megaparsec.Char.Lexer)


{- Takes a parser of type a, as an argument runs this parser, 
   then uses skipSpace defined above to ignore 
   any whitespace or comments following the Parser -}
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme skipSpace -- from (Text.Megaparsec.Char.Lexer)


{- Allows the parsing of both intergers and floats. 
   First attempts to parse the signed float number.
   (try) is vital, as if it fails, and is not of type float. 
   The  method will try again using and integer number  -}
myFloat :: Parser Float
myFloat =
    {- (Lexer.signed) allows the parsing of negative and positive
       skipSpace used to ignore any whitespace/comments between sign and number -}
    try (Lexer.signed skipSpace Lexer.float)
    {- (<|>) allows the function to try the other option
       If first part failed means we have an integer, and need to
       convert our integer to float using (fromIntegral). 
       Explicit type decleration of fromIntegral to remove warning -}
    <|> (fromIntegral :: Integer -> Float) <$> Lexer.signed skipSpace Lexer.decimal


{- (void) discards the result of the parser, from Control.Monad 
   (many) combinator applies the parser 0 or more times, 
   (newline) will ignore any consecutive newline characters
   (>>) used to combine and discard 2 sequences, 
   newline is ignored first then will skip and comments or whitespace -}
endLine :: Parser ()
endLine = void $ many newline >> skipSpace


{- Helper function to parse a hogo command which takes a float number, 
   this was done to limit repetitive code and improve readability of code.
   (str) is the string used to identify which HogoCode is being called
   (command) is the HogoCode that the function is parsing
   (<$>) applies the result of parsing the strings to the Hogo command, 
   for example to form (GoForward 10)
   (symbol str) parses the speific string associated with the Hogo command
   (*>) which evaluates the string and the (myFloat) and discards the result of string
   (<*) this returns the float and discards the whitespace/newline after it -}
floatCommand :: String -> (Float -> HogoCode) -> Parser HogoCode
floatCommand str command = command <$> (symbol str *> myFloat <* endLine)


{- Helper function to parse a hogo command which takes doesnt take a number, 
   this was done to limit repetitive code and improve readability of code.
   (str) is the string used to identify which HogoCode is being called
   (command) is the HogoCode that the function is parsing
   (<$) this is utilised to parse the string, and if it is successfully parsed, 
   the result of the string is ignored and the command is returned, 
   this is done for commands which dont need the strings calling it, 
   and only need to evaluate whether the string actually matches the Hogo command
   (symbol str) parses the speific string associated with the Hogo command
   (<*) this returns the string and discards the whitespace/newline after it -}
basicCommand :: String -> HogoCode -> Parser HogoCode
basicCommand str name = name <$ (symbol str <* endLine)


{- Parsing function to repeat Hogo commands called by "repeat" and number of repeats 
   Although, I would normally use combinators over do notation, for this function
   for readability and elegance i decided to utilise do notation. 
   The string is parsed and ignored, only called by "repeat" keyword, 
   (symbol) consumers trailing whitespace, therefore we can call lexeme, 
   which directly parses the number of repetitions which is being commanded, 
   this value is important and is assigned to repititions. 
   Next utilisation of (between) to retrieve code between "[" and "]", 
   (many) combinator applies the parser 0 or more times to the strings between "[]"  
   and all commands are assessed individually, where the trailing whitespace is ignored
   using (<*) which returns the value of the parseCommand and discards the unneccessary space. 
   Finally pure combines the pieces together to form the complete HogoCommand for Repeat. 
    -}
repeatCommand :: Parser HogoCode
repeatCommand = do
    _ <- symbol "repeat" -- value discarded only used to confirm called by correct string
    repetitions <- lexeme Lexer.decimal -- store value of parsing the trailing number
    hogoCommands <- between (symbol "[") (symbol "]") (many $ parseCommand <* endLine)
    pure (Repeat repetitions hogoCommands) -- returns combined HogoCode


{- I utilised helper functions for this function to improve readability, 
   and limit the amount of repetitive code I was forming. 
   (floatCommand) uses the indicated string to parse it to the HogoCommand specified 
   likewise with (basicCommand), and repeatCommand which finds strings attributed 
   to HogoCode between "[]". This is all being cycled using choice, which 
   checks each parser and return when the given string has been parsed correctly -}
parseCommand :: Parser HogoCode
parseCommand = choice
    [ floatCommand "forward" GoForward
    , floatCommand "back" GoBackward
    , floatCommand "left" TurnLeft
    , floatCommand "right" TurnRight
    , basicCommand "penup" PenUp
    , basicCommand "pendown" PenDown
    , basicCommand "clearscreen" ClearScreen
    , basicCommand "home" GoHome
    , repeatCommand
    ]


{- I used do notation to parse the size of the maze as i feel like it is an 
   idiomatic and readable way to display comnsuming whitespace and then read the string value
   (<$>) applies read to a parsed string inputs to convert them into integers. 
   (When) from Control.Monad used to check if the parsed mazeSize is in range -}
parseMazeSize :: Parser Int
parseMazeSize = do
    _ <- skipSpace -- ignore any whitespace user may have accidentally pressed
    mazeSize <- read <$> some digitChar -- read the input converting into integer
    when (mazeSize < 2 || mazeSize > 100) $ -- if input not in range fail else return size
        fail "Incorrect size sellection, choose a Maze between (2 - 100) to continue:"
    pure mazeSize


{- Parser for colors that turtle draws, limited by Gloss library, 
   red is not included as red is used by the optimal (PathFinder) 
   to illustrate the optimal path after the game has ended. 
   Utilises choice which cycles through options and if all colors fail, 
   print the error message which asks for a color of correct range -}
parseTurtleColor :: Parser Color
parseTurtleColor = choice
    [ try (string "black") >> pure black
    , try (string "green") >> pure green
    , try (string "yellow") >> pure yellow
    , try (string "blue") >> pure blue
    , try (string "cyan") >> pure cyan
    , try (string "magenta") >> pure magenta
    ]
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.WebMain
    ( generateMazeJS
    , solveMazeJS
    ) where

import Web.MazeLogic (generateMaze, solveMaze, Position, Maze)
import GHCJS.Types (JSVal)
import GHCJS.Marshal (toJSVal)
import Data.Aeson (encode, toJSON, object, (.=))
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- | Generate maze and return as JSON
generateMazeJS :: Int -> Int -> IO JSVal
generateMazeJS width height = do
    let maze = generateMaze width height
    let jsonResult = object
            [ "maze" .= maze
            , "width" .= width
            , "height" .= height
            , "success" .= True
            ]
    toJSVal $ toStrict $ decodeUtf8 $ encode jsonResult

-- | Solve maze and return path as JSON
solveMazeJS :: Int -> Int -> IO JSVal
solveMazeJS width height = do
    let maze = generateMaze width height
    let path = solveMaze maze
    let jsonResult = object
            [ "path" .= path
            , "maze" .= maze
            , "pathLength" .= length path
            , "success" .= (not $ null path)
            ]
    toJSVal $ toStrict $ decodeUtf8 $ encode jsonResult

-- Foreign exports for JavaScript
foreign export javascript "generateMazeJS" generateMazeJS :: Int -> Int -> IO JSVal
foreign export javascript "solveMazeJS" solveMazeJS :: Int -> Int -> IO JSVal
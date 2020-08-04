module Main where

import Snake

loop :: State -> IO ()
loop state = do
    putStr $ show state
    move <- getLine
    loop $ nextState (registerMove state (read move :: Direction))

main :: IO ()
main = do
    putStrLn "Starting snake game..."
    loop defaultState
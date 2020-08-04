module Main where

import Snake
import System.Random

loop :: State -> [Point] -> IO ()
loop state (x:xs) = do
    putStr $ show state
    move <- getLine
    loop (nextState (registerMove state (read move :: Direction)) x) xs
loop _ invalidRandoms = undefined

main :: IO ()
main = do
    gen <- getStdGen
    let (genW, genH) = split gen
        randomWidths = randomRs (0, width defaultState) genW
        randomHeights = randomRs (0, height defaultState) genH
    putStrLn "Starting snake game..."
    loop defaultState (zip randomWidths randomHeights)
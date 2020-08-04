module Snake where

import System.Random

type Point = (Int, Int)
type Apple = (Int, Int)
type Snake = [Point]

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Show)

data State = State {
    snake :: Snake,
    apple :: Apple,
    width :: Int,
    height :: Int,
    moves :: [Direction]
} deriving (Show)

defaultState = (State {snake=[(1, 4), (1, 3), (1, 2)], apple=(5,5), width=5, height=5, moves=[UP, LEFT]})

-- nextState state = State {
--     snake = nextSnake state,
--     apple = nextApple state,
--     width = width state,
--     height = height state,
--     moves = nextMoves state
-- }


nextApple :: State -> Apple
nextApple state
    | willCrash state = apple state
    | otherwise = apple state

addDirection :: Direction -> Point -> Point
addDirection UP = addTuple (0,-1)
addDirection DOWN = addTuple (0,1)
addDirection LEFT = addTuple (1,0)
addDirection RIGHT = addTuple (-1,0)

addTuple :: Point -> Point -> Point
addTuple (a,b) (c,d) = (a+c,b+d)

nextHead :: State -> Point
nextHead state = wrapPoint state (addDirection (head $ moves state) (head $ snake state))

wrapPoint :: State -> Point -> Point
wrapPoint state (x,y) = (x `mod` (width state), y `mod` (height state))

willCrash :: State -> Bool
willCrash state
    | any (== (nextHead state)) (snake state) = True
    | otherwise = False

willEat :: State -> Bool
willEat state = nextHead state == apple state

nextSnake :: State -> Snake
nextSnake state
    | willEat state = nextHead state : snake state
    | otherwise = nextHead state : init (snake state)

nextMoves :: State -> [Direction]
nextMoves State{moves=[x]} = [x]
nextMoves State{moves=(x:xs)} = xs
nextMoves State{moves=[]} = []

randomList :: Int -> [Double]
randomList seed = randoms (mkStdGen seed)



-- main = do
--     seed <- randomIO
--     let
--         randoms = randomList seed
--         loop = 
--     in 

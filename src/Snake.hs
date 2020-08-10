module Snake where

import Data.List
import Debug.Trace

type Point = (Int, Int)
type Apple = (Int, Int)
type Snake = [Point]

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq, Show, Read)

data State = State {
    snake :: Snake,
    apple :: Apple,
    width :: Int,
    height :: Int,
    moves :: [Direction]
}

instance Show State where
    show s = (intercalate "\n" [[tile x y | x <- [0..w]] | y <- [0..h]]) ++ "\n"
        where
            tile x y
                | (x,y) `elem` (snake s) = 'x'
                | (x,y) == (apple s) = 'o'
                | (x == 0 || x == w) && (y == 0 || y == h) = '+'
                | (x == 0 || x == w) = '|'
                | (y == 0 || y == h) = '-'
                | otherwise = ' '
            w = width s + 1
            h = height s + 1

defaultState = (State {snake=[(3, 2), (3, 3), (3, 4), (3, 5), (3, 6)], apple=(1,5), width=20, height=10, moves=[UP]})

nextState :: State -> Apple -> State
nextState state randomApple
    | willCrash state = defaultState
    | otherwise = progressState state
        where progressState = do
                snake <- nextSnake
                apple <- nextApple randomApple
                width <- width
                height <- height
                moves <- nextMoves
                return (State{snake, apple, width, height, moves})

nextApple :: Point -> State -> Apple
nextApple randomPoint state
    | willEat state = randomPoint
    | otherwise = apple state

addDirection :: Direction -> Point -> Point
addDirection UP = addTuple (0,-1)
addDirection DOWN = addTuple (0,1)
addDirection LEFT = addTuple (-1,0)
addDirection RIGHT = addTuple (1,0)

-- see if there is a better way to do the pattern matching on this?
opposite :: Direction -> Direction -> Bool
opposite UP DOWN = True
opposite DOWN UP = True
opposite LEFT RIGHT = True
opposite RIGHT LEFT = True
opposite _ _ = False

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

-- It would be good if moves was actually a sequence in this case, rather than a list
registerMove :: State -> Direction -> State
registerMove s@(State{moves=m}) move
    | (opposite (head m) move) = s
    | otherwise = s{moves=([move])}

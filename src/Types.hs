module Types where

import Data.Char (toUpper, ord)
import Data.List (intercalate, transpose)
import Control.Concurrent (yield)

_SIZE_ :: Int
_SIZE_ = 8

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

convertRowIndex :: Char -> Int
convertRowIndex x = fromEnum(toUpper x) - 64

_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

_SEP_ :: [Char]
_SEP_ = "_|_"

data Square = B | W | Empty deriving(Show, Eq)

data GameState = BWon | WWon | Tie | InProgress deriving Show

checkGameState t = case t of
     BWon -> "Black  won the game!"
     WWon -> "White won the game"
     Tie -> "The game is a tie!" 
     InProgress -> "The game is in progress..."


type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

getFirstPlayer :: Bool -> Player
getFirstPlayer bool = 
    if bool == True  then B
    else W

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ bool
    | True  = B
    | otherwise = W

showGameState :: GameState -> String
showGameState gs = case gs of
    BWon            ->       "Black won the game!"
    WWon            ->       "White won the game"
    Tie             ->       "The game is a tie!" 
    InProgress      ->       "The game is in progress..."

switchPlayer :: Player -> Player
switchPlayer B = W
switchPlayer W = B
switchPlayer Empty = Empty

showSquare :: Square -> String
showSquare B = "B"
showSquare W = "W"
showSquare  Empty = "_"

------------------------------

-- Q#01

promptPlayer :: Player -> String
promptPlayer Empty = "No player's turn"
promptPlayer p = concat ["Player ", show p, "'s turn: enter a row and column position " ]

-- Q#02
_RANGE_ ::  [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c = if isDigit c then read [c] else -1

-- Q#04

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied b = Empty `notElem` concat b

_NEW_BOARD_ = [
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, B, W, Empty, Empty, Empty]
  , [Empty, Empty, Empty, W, B, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  ]
n = _NEW_BOARD_

_TIED_BOARD_ :: Board 
_TIED_BOARD_ = [
    [B, B, B, W, W, W, B, W]
  , [W, W, W, B, B, W, B, B]
  , [B, W, W, B, W, W, W, B]
  , [B, W, W, B, W, B, B, W]
  , [B, B, W, B, W, B, W, B]
  , [B, B, B, B, W, W, W, W]
  , [B, W, B, B, W, B, W, B]
  , [B, B, B, W, W, W, W, W]
  ]

t = _TIED_BOARD_

-- Q#06

indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings []  = []
indexRowStrings s = zip ['A'.. ] s

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = checkX && checkY
  where
    checkX = (x >= 0) && (x < _SIZE_)
    checkY = (y >= 0) && (y < _SIZE_)

-- Q#09

stringToMove :: String -> Move
stringToMove [r, c] = (convertRowIndex r - 1, readDigit c)
stringToMove [r, c] = (readDigit r,  readDigit c)
stringToMove [r, c] = ( ((convertRowIndex r) - 1), ((convertRowIndex c) - 1) )
stringToMove _      = _INVALID_MOVE_

------------------------------

-- *** Assignment 3-1 ***

-- Q#01

------------------------------

showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ = ' ' : formatLine(showInts _RANGE_)

showSquares :: [Square] -> [String]
showSquares (x:xs) = showSquare x : showSquares xs
showSquares [] = []

formatLine :: [String] -> String
formatLine s = _SEP_ ++ intercalate _SEP_ s ++ _SEP_

formatRows :: [Row] -> [String]
formatRows b = map (formatLine . showSquares) b

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (:) ['A'..] s

formatBoard :: Board -> String
formatBoard b = unlines(_HEADER_ : prependRowIndices(formatRows b))

printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

------------------------------

promptPlayer :: Player -> String
promptPlayer Empty = concat ["No player's turn" ]
promptPlayer p = concat ["Player ", show(p), "'s turn: enter a row and column position " ]

module Types where

import Data.Char (toUpper, ord)
import Data.List (intercalate, transpose)
import Control.Concurrent (yield)
import Data.Char (isAlpha)

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
-- Q#09

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

------------------------------

-- Select a piece with Coordinates

stringToMove :: String -> Move
stringToMove [r, c]
  | isAlpha r && isDigit c = (convertRowIndex r - 1, readDigit c)
  | isDigit r && isDigit c = (readDigit r,  readDigit c)
  | isAlpha r && isAlpha c = ( ((convertRowIndex r) - 1), ((convertRowIndex c) - 1) )
  | isDigit r && isAlpha c = ( readDigit r, ((convertRowIndex c) - 1) )
  | otherwise              = _INVALID_MOVE_

isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = checkX && checkY
  where
    checkX = (x >= 0) && (x < _SIZE_)
    checkY = (y >= 0) && (y < _SIZE_)

isValidPiece :: Player -> Board -> Move -> Bool
isValidPiece p b (i, j) = isMoveInBounds (i, j) && go p r (i, j)
  where
    go :: Player -> [Square] -> Move -> Bool
    -- calls on the specific row
    r =  head( drop i b)
    -- if the current column is Empty then it is a valid move
    go p r (i, 0) = head r == p
    -- recursively drops columns until the called on column is selected
    go p r (i, j)  = go p (drop 1 r) (i, j-1)

checkSquare :: Board -> Move -> Square
checkSquare b (r, c) = boardCoord dropR (r, c)
  where
    -- drops to correct row
    dropR = head $ drop r b
    --
    boardCoord dropR (r, 0) = head dropR
    -- recursively drops to correct column
    boardCoord dropR (r, c) = boardCoord (drop 1 dropR) (r, c-1)


-- Validate moves the piece can go. Type in N,NE,E,SE,S,SW,W,NW ... To Do...
-- create seperate cases for N, NE, etc.  N (x doesn't chnage, y only increases)
-- NE (when x increases, y increases)
-- check if piece in that direction is an opposing piece, recursively check in direction that piece
  -- is opposing piece until reaching Empty, Player piece is in way (Try again), or move is out of bounds (try again)

data Direction = N | NE | E | SE | S | SW | We | NW deriving(Show, Eq)

directionCoord :: Move -> [(Direction, (Int, Int))]
directionCoord (i, j) = zipWith (,) [N, NE, E, SE, S, SW, We, NW] [(i, j+1), (i+1, j+1), (i+1, j), (i+1, j-1), (i, j-1), (i-1, j-1), (i-1, j), (i-1, j+1)]

nextToOppPlayer :: Board -> Move -> [(Direction, (Int, Int))]

nextToOppPlayer b m = <*> (directionCoord m) 
-- ismoveInBounds && checkOppPlayer && checkDirection (Checks for empty, oppPlayer recursively, or samePlayer)

-- Need function to check opposing player is next player piece in specified direction 
-- Need function to check the player based on coordinates

{- checkDirection :: Player -> Board -> Move -> Direction -> Bool
checkDirection p (i,j) d = (isValidPiece p b (i,j)) && (go p (i,j) d) 
  where
    go :: Player -> Move -> Direction -> Bool
    go = case d of
    N = if (i, j+1) == (switchPlayer p) then checkDirection p (i,j+1) N elsif (i, j+1) == p then False elseif (i, j+1) == Empty then True
 -}

isValidMove :: Board -> Move -> Bool
isValidMove b (i, j) = isMoveInBounds (i, j) && go r (i, j)
  where
    -- calls on the specific row
    r =  head( drop i b)
    -- if the current column is Empty then it is a valid move
    go  r (i, 0) = head r == Empty
    -- recursively drops columns until the called on column is selected
    go  r (i, j)  = go (drop 1 r) (i, j-1)


{- getMove :: Board -> IO Move
getMove b = getLine >>= worker . stringToMove
    where
        worker :: Move -> IO Move
        worker m = if isValidMove b m
                      then return m
                      else putStrLn "Invalid move! Try again" >> getMove b
 -}

  
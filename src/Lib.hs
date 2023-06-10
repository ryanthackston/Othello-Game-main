module Lib where

import Types
import Actions

import Data.Char (toUpper, ord)
import Data.List (intercalate, transpose)
import Control.Concurrent (yield)
import Data.Char (isAlpha)
import Data.Bifunctor (second)

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

_RANGE_ ::  [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

checkGameState t = case t of
     BWon -> "Black  won the game!"
     WWon -> "White won the game"
     Tie -> "The game is a tie!"
     InProgress -> "The game is in progress..."

_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

e = _EMPTY_BOARD_

getFirstPlayer :: Bool -> Player
getFirstPlayer bool
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

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit c = if isDigit c then read [c] else -1

-- Q#05

isTied :: Board -> Bool
isTied b = Empty `notElem` concat b

_NEW_BOARD_ = transpose [
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

indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings []  = []
indexRowStrings s = zip ['A'.. ] s


showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

showSquares :: [Square] -> [String]
showSquares (x:xs) = showSquare x : showSquares xs
showSquares [] = []

formatLine :: [String] -> String
formatLine s = _SEP_ ++ intercalate _SEP_ s ++ _SEP_

formatRows :: [Row] -> [String]
formatRows b = map (formatLine . showSquares) b

prependRowIndices :: [String] -> [String]
prependRowIndices s = zipWith (:) ['A'..] s

_HEADER_ :: String
_HEADER_ = ' ' : formatLine(showInts _RANGE_)

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


-- Validate moves the piece can go. Type in N,NE,E,SE,S,SW,W,NW
-- create seperate cases for N, NE, etc.  N (x doesn't change, y only increases)
-- NE (when x increases, y increases)
-- check if piece in that direction is an opposing piece, recursively check in direction that piece
  -- is opposing piece until reaching Empty, Player piece is in way (Try again), or move is out of bounds (try again)

data Direction = N | NE | E | SE | S | SW | We | NW deriving(Show, Eq)

directionCoord :: Move -> [(Direction, (Int, Int))]
directionCoord (i, j) = zipWith (,) [S, SE, E, NE, N, NW, We, SW] [(i+1, j), (i-1, j-1), (i, j+1), (i-1, j+1), (i, j-1), (i+1, j+1), (i-1, j), (i+1, j-1)]

{- directionCoord :: Move -> [(Direction, (Int, Int))]
directionCoord (i, j) = zipWith (,) [S, SE, E, NE, N, NW, We, SW] [(i+1, j), (i-1, j-1), (i, j+1), (i+1, j-1), (i, j-1), (i+1, j+1), (i-1, j), (i-1, j+1)]
 -}
idDirCoord :: [(Direction, (Int, Int))]
idDirCoord = directionCoord (0,0)

z = directionCoord (3,3)

-- Use splitAt to take out undesireable directions
-- use unzip to seperate the zipped variables

{- nextToOppPlayer :: Board -> Move -> [(Direction, (Int, Int))]

nextToOppPlayer b m = <*> (directionCoord m)  -}
-- ismoveInBounds && checkOppPlayer && checkDirection (Checks for empty, oppPlayer recursively, or samePlayer)


-- Need function to check opposing player is next player piece in specified direction 

minusTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
minusTuple (x1, y1) (x2, y2) = (x2-x1, y2-y1)

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1, y1) (x2, y2) = (x2+x1, y2+y1)

accumTuple :: Move -> (Int, Int) -> (Int, Int) -> (Int, Int)
accumTuple (x1, y1) (x2, y2) (x3, y3)= addTuple (x3, y3) (minusTuple (x1, y1) (x2, y2))

-- second is the same as fmap or bimap. 
-- fmap applied to the coordinates

--squareDir (3,3) snd
{- squareDir :: (Int, Int) -> (Int, Int) -> (Int, Int)
squareDir _ (i, j) | i < -1 && j < -1 && i > 1  && j > 1 = _INVALID_MOVE_
squareDir (x, y) _ | x < 0 || x > _SIZE_ - 1 || y < 0 || y > _SIZE_ - 1 = _INVALID_MOVE_
squareDir (x,y) (i, j) = (x+i, y+j) -}

{- outOfBounds :: Move -> (Int, Int) -> Bool
outOfBounds m1 m2 -}

validDirections :: Board -> Move -> [(Direction, (Int, Int))] -> [(Direction, (Int, Int))]
validDirections _ _ []     = []
validDirections b m (t:ts) = if checkSquare b (snd t) == switchPlayer (checkSquare b m) then t : validDirections b m ts else validDirections b m ts

-- Bug with empty list here
checkNextSquare :: Board -> Move -> (Int, Int) -> (Direction, (Int, Int)) -> Square
checkNextSquare _ (r1,c1) (r2, c2) _ | any ((== True) . (> _SIZE_ - 1)) [r1, c1, r2, c2]  = error "Out of Bounds"
checkNextSquare _ (r1,c1) (r2, c2) _ | any ((== True) . (< 0)) [r1, c1, r2, c2]  = error "Out of Bounds"
checkNextSquare b m1 (r2, c2) _ | any ((== True) . (== _SIZE_ -1 )) [r2, c2]  = checkSquare b (r2, c2)
checkNextSquare b m1 m2 t   = checkSquare b (accumTuple m1 (snd t) m2)

-- Maybe Function
headDirection :: Board -> Move -> [(Direction, (Int, Int))] -> (Direction, (Int, Int))
headDirection _ _ []     = error "This is an empty list"
headDirection b m (t:ts) = head (validDirections b m (t:ts))

accumDirection :: Move -> (Int, Int) -> (Direction, (Int, Int)) -> Move
accumDirection m1 m2 t = accumTuple m1 (snd t) m2 

-- Need to fix the exception
validLines :: Board -> Move -> (Int, Int) -> Player -> [(Direction, (Int, Int))] -> [(Direction, (Int,Int))]
validLines _ _  _  _ [] = []
validLines b m1 (r2, c2) p (t:ts) | any ((== True) . (> _SIZE_ - 1)) [r2, c2] ||  any ((== True) . (< 0)) [r2, c2]  = validLines b m1 m1 p ts
validLines b m1 (r2, c2) p (t:ts)
      | isMoveInBounds (r2, c2) && (checkNextSquare b m1 (r2, c2) (headDirection b m1 (t:ts)) == Empty) = t : validLines b m1 m1 p ts
      | isMoveInBounds (r2, c2) && (checkNextSquare b m1 (r2, c2) (headDirection b m1 (t:ts)) == p) = validLines b m1 m1 p ts
      | isMoveInBounds (r2, c2) && (checkNextSquare b m1 (r2, c2) (headDirection b m1 (t:ts)) == switchPlayer p) = validLines b m1 (accumTuple m1 (snd t) (r2, c2) ) p (t:ts)
      | otherwise = validLines b m1 m1 p ts

t1 = [
    [W, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, W, Empty, B, Empty, W, Empty, Empty]
  , [Empty, Empty, W, B, W, Empty, Empty, Empty]
  , [Empty, Empty, Empty, B, W, W, W, Empty]
  , [Empty, Empty, Empty, W, B, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  ]

------------------------------
-- Select your Direction

chooseDir :: Move -> Direction -> (Direction, (Int, Int))
chooseDir m d = head $ filter ((== d).fst) (directionCoord m)

-- Unsafe head
-- Change the Filter function. Break up the function more and make simpler. 

-- Don't need to use filter & take head, try `elem` which is a safe function


accDir :: Direction -> (Int, Int)
accDir d = snd (head (filter ((== d).fst) (directionCoord (0,0))))

-- Bug with empty list error at the end. Rewrite with a countdown
-- Function could have empty values
getSquares :: Board -> Player -> Move -> Direction -> [Square]
getSquares b p m d = checkSquare b m : go b p m d
  where
    go :: Board -> Player -> Move -> Direction -> [Square]
    go b p (r, c) d | any ((== True) . (> _SIZE_ - 1)) [r, c] = []
    go b p (r, c) d | any ((== True) . (< 0)) [r, c] = []
    go b p (r, c) d = checkSquare b (snd(chooseDir (r, c) d)) : go b p (addTuple (r, c) (accDir d)) d

------------------------------

-- Replace pieces in Selected Direction

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p c r = xs ++ ys'
  where
    (xs, ys) = splitAt c r
    ys'
      | null ys = []
      | c < 0     = ys
      | otherwise = p : tail ys

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (r:rs) (0, j) = let r' = replaceSquareInRow p j r in r':rs
putSquare p b@(r:rs) m@(i, j)
  | i > 0 = r : putSquare p rs (i-1, j)
  | otherwise = b



{- replaceSquare :: Player -> Board -> Move -> Board
replaceSquare p b (r, c) =  -} 



-- filter ((== d).fst) zz
{- playDirection :: Board -> Move -> Direction -> [(Direction, (Int, Int))] -> Board
playDirection _ _       []          = []
playDirection b userDir [(d, (r, c))] = filter ((== userDir).fst) [(d, (r,c))]  -}

t2 =  [
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, W, Empty, Empty]
  , [Empty, Empty, Empty, Empty, W, Empty, Empty, Empty]
  , [Empty, Empty, Empty, B, W, W, W, Empty]
  , [Empty, Empty, Empty, Empty, B, Empty, Empty, Empty]
  , [Empty, Empty, Empty, B, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  ]

tz = validDirections t2 (3,3) z

------------------------------

-- Start working on IO and bring everything together

------------------------------

-- Once I have Direction and Coordinate, I need to 

-- putSquare each 


-- Check for Empty Square in chosen direction
-- checkForEmpty :: [(Direction, (Int, Int))] -> [(Direction, (Int, Int))]

{- checkDirection :: Player -> Board -> Move -> Direction -> Bool
checkDirection p (i,j) d = (isValidPiece p b (i,j)) && (go p (i,j) d) 
  where
    go :: Player -> Move -> Direction -> Bool
    go = case d of
    N = if (i, j+1) == (switchPlayer p) then checkDirection p (i,j+1) N elsif (i, j+1) == p then False elseif (i, j+1) == Empty then True
 -}

-- 
{- getMove :: Board -> IO Move
getMove b = getLine >>= worker . stringToMove
    where
        worker :: Move -> IO Move
        worker m = if isValidMove b m
                      then return m
                      else putStrLn "Invalid move! Try again" >> getMove b
 -}
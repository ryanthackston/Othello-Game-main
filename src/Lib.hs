module Lib where

import Types
import Actions

import Data.Char ( toUpper, ord, isAlpha )
import Data.List (intercalate, transpose, elemIndex)
import Control.Concurrent (yield)
import Data.Bifunctor (second)
import Control.Monad (when)
import qualified Data.Set as S


-- Basic Functions for setting up the board

-- 8x8 size board

_SIZE_ :: Int
_SIZE_ = 8

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./logo.txt"

printLogo :: IO String
printLogo = readFile _LOGO_PATH_

convertRowIndex :: Char -> Int
convertRowIndex x = fromEnum(toUpper x) - 65

_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Seperates squares on the board
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
showSquare Empty = "_"

-- check if character is a digit
isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

-- 
readDigit :: Char -> Int
readDigit c = if isDigit c then read [c] else -1


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
showInts (x:xs) = show x : showInts xs
showInts xs = map show xs

showSquares :: [Square] -> [String]
showSquares (x:xs) = showSquare x : showSquares xs
showSquares xs = map show xs

formatLine :: [String] -> String
formatLine s = _SEP_ ++ intercalate _SEP_ s ++ _SEP_

formatRows :: [Row] -> [String]
formatRows = map (formatLine . showSquares)

prependRowIndices :: [String] -> [String]
prependRowIndices = zipWith (:) ['A'..]

_HEADER_ :: String
_HEADER_ = ' ' : formatLine(showInts _RANGE_)

formatBoard :: Board -> String
formatBoard b = unlines(_HEADER_ : prependRowIndices(formatRows b))

-- Prints the game board in terminal
printBoard :: Board -> IO ()
printBoard b = putStrLn $ formatBoard b

------------------------------
-- Prompt the players turn

promptPlayer :: Player -> String
promptPlayer Empty = "No player's turn"
promptPlayer p = concat ["Player ", show(p), "'s turn: enter a row and column position (letter and number)" ]

------------------------------

-- Select a piece with Coordinates

stringToMove :: [Char] -> Move
stringToMove [] = _INVALID_MOVE_
stringToMove [c1] = _INVALID_MOVE_
stringToMove (c1:c2)
  | isAlpha c1 && isDigit (head c2) = (convertRowIndex c1, readDigit (head c2))
  | isDigit c1 && isDigit (head c2) = (readDigit c1,  readDigit (head c2))
  | isAlpha c1 && isAlpha (head c2) = ( convertRowIndex c1, convertRowIndex (head c2))
  | isDigit c1 && isAlpha (head c2) = ( readDigit c1, convertRowIndex (head c2) )
  | otherwise                       = _INVALID_MOVE_

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

-- isMoveInBounds (stringToMove "a7")
-- isValidPiece B t1 (stringToMove "C3")

checkSquare :: Board -> Move -> Player
checkSquare b (r, c) = boardCoord dropR (r, c)
  where
    -- drops to correct row
    dropR = head $ drop r b
    --
    boardCoord dropR (r, 0) = head dropR
    -- recursively drops to correct column
    boardCoord dropR (r, c) = boardCoord (drop 1 dropR) (r, c-1)

-- Select where to place move, check if the piece is an empty square.

filterOutBounds :: Move -> [Move]
filterOutBounds m = map snd (filter ((== True) . fst) (zip (isMoveInBounds <$> (addTuple m <$> dir)) (addTuple m <$> dir)))

-- Check if there's an opposing piece next to the selected coordinate
checkNextBool :: Player -> Board -> Move -> Bool
checkNextBool p b m =  notElem (switchPlayer p) (checkSquare b <$> (filterOutBounds m))

validSquare :: Player -> Board -> Move -> Bool
validSquare p b (r,c) = if (checkSquare b (r,c) /= Empty) || checkNextBool p b (r,c) then False else True  

directionCoord :: [(Int, Int)]
directionCoord = [(1, 0), (-1, -1), (0, 1), (-1, 1), (0, -1), (1, 1), (-1, 0), (1, -1)]

dir = directionCoord

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (x1, y1) (x2, y2) = (x2+x1, y2+y1)

-- After checking if it's a valid square, I
-- Recursively check opposing Player Squares until hit Empty, same Player, or end of board

-- Add every piece that can be changed to a list 


{- checkMove :: Player -> Board -> Move  -> [(Int, Int)] -> [(Int, Int)]
checkMove _ _ _     []     = []
checkMove p b (r,c) (d:ds) = checkLine p b (r,c) d ++ checkMove p b (addTuple (r,c) d) ds
  where
    checkLine :: Player -> Board -> Move -> (Int, Int) -> [(Int, Int)]
    checkLine p b (r,c) d
      | isMoveInBounds (addTuple (r,c) d) && (checkSquare b (addTuple (r,c) d) == switchPlayer p) =  (addTuple (r,c) d): checkLine p b (addTuple (r,c) d)
      | isMoveInBounds (addTuple (r,c) d) && (checkSquare b (addTuple (r,c) d) == p) = checkMove p b (addTuple (r,c) ds)
      | isMoveInBounds (addTuple (r,c) d) && (checkSquare b (addTuple (r,c) d) == Empty) = (addTuple (r,c) d) : checkMove p b (addTuple (r,c) ds) -}

{- checkMove :: Player -> Board -> Move  -> [(Int, Int)] ->  [(Int, Int)]
checkMove _ _ _     []     = []
checkMove p b (r,c) (d:ds) = checkNext p b (r,c) d 
  where
    checkNext :: Player -> Board -> Move -> (Int, Int) -> [(Int, Int)]
    checkNext p b (r,c) d 
      | isMoveInBounds (addTuple (r,c) d) && (checkSquare b (addTuple (r,c) d) == switchPlayer p) = addTuple (r,c) d : checkLine p b (addTuple (addTuple (r,c) d) d) d
      | otherwise = checkMove p b (r,c) ds
      where
        checkLine :: Player -> Board -> Move -> (Int, Int) -> [(Int, Int)]
        checkLine p b (r2,c2) d
         | isMoveInBounds (r2,c2)  && checkSquare b (r2,c2) == switchPlayer p =  (r2,c2)  : checkLine p b (addTuple (r2,c2) d) d
         | isMoveInBounds (addTuple (r2,c2) d) && (checkSquare b (r2,c2) == p) =  checkMove p b (r,c) ds
         | isMoveInBounds (addTuple (r2,c2) d) && (checkSquare b (r2,c2) == Empty) = checkMove p b (r,c) ds
         | otherwise = checkMove p b (r,c) ds -}

-- create sublists of piece placement lists. Invalid lists with an empty square or are out-of-bounds end with a (-1, -1) Tuple.
-- Invalid lists that don't start with opposing player piece have empty list []
-- valid squares ending with the same player square have a complete list ending with []  
checkMove :: Player -> Board -> Move  -> [(Int, Int)] ->  [[(Int, Int)]]
checkMove _ _ _     []     = []
checkMove p b (r,c) (d:ds) = checkNext p b (r,c) d : checkMove p b (r,c) ds
  where
    checkNext :: Player -> Board -> Move -> (Int, Int) -> [(Int, Int)]
    checkNext p b (r,c) d 
      | isMoveInBounds (addTuple (r,c) d) && (checkSquare b (addTuple (r,c) d) == switchPlayer p) = addTuple (r,c) d : checkLine p b (addTuple (addTuple (r,c) d) d) d
      | otherwise = []
      where
        checkLine :: Player -> Board -> Move -> (Int, Int) -> [(Int, Int)]
        checkLine p b (r2,c2) d
          | isMoveInBounds (r2,c2)  && checkSquare b (r2,c2) == switchPlayer p =  (r2,c2)  : checkLine p b (addTuple (r2,c2) d) d
          | isMoveInBounds (addTuple (r2,c2) d) && (checkSquare b (r2,c2) == p) =  []
          | isMoveInBounds (addTuple (r2,c2) d) && (checkSquare b (r2,c2) == Empty) = [(-1,-1)]
          | otherwise = [(-1,-1)]

-- Filter out lists with (-1, -1) tuple in list and empty lists [].
-- concatenate inital move and lists together into one list.
-- test = checkMove B t1 (2,6) dir
filterLists :: [[(Int, Int)]] -> [(Int, Int)]
filterLists lists = concat (filter (notElem (-1, -1)) (filter (not.null) lists))

checkFilterLists :: Move -> [(Int, Int)] -> [(Int, Int)]
checkFilterLists (r,c) list = if length list > 1 then (r,c) : list else []

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p c r = xs ++ ys'
  where
    (xs, ys) = splitAt c r
    ys'
      | null ys = []
      | c < 0     = ys
      | otherwise = p : tail ys

-- Player pieces added to valid piece squares
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (r:rs) (0, j) = let r' = replaceSquareInRow p j r in r':rs
putSquare p b@(r:rs) m@(i, j)
  | i > 0 = r : putSquare p rs (i-1, j)
  | otherwise = b

putSquares :: Player -> Board -> [Move] -> Board 
putSquares _ b []     = b
putSquares p b (m:ms) = putSquares p (putSquare p b m) ms

-------

-- Make IO Code to play

getMove :: Player -> Board -> IO Move
getMove p b = getLine >>= worker . stringToMove
    where
        worker :: Move -> IO Move
        worker m = if validSquare p b m
                      then return m
                      else putStrLn "Invalid move! Try again" >> getMove p b


countSquares :: Board -> (Int, Int)
countSquares b = (length $ filter (== W) (concat b), length $ filter (== B) (concat b))

getGameState :: Board -> GameState
getGameState b
  -- Any empty tile means game in progress
  | elem Empty (concat b) = InProgress
  | uncurry (>) (countSquares t1) = WWon
  | uncurry (<) (countSquares t1) = BWon
  | otherwise = Tie

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState(putSquares p b (filterLists (checkMove p b m dir)) ), putSquares p b (filterLists (checkMove p b m dir)))

play :: Player -> Board -> IO ()
play p b = when _DISPLAY_LOGO_ (printLogo >>= putStrLn)  >> printBoard b >> putStrLn (promptPlayer p) >> getMove p b >>= executeMove
    where
        executeMove :: Move -> IO ()
        executeMove m = let (newState, newBoard) = playMove p b m
            in case newState of
                InProgress -> play (switchPlayer p) newBoard
                _ -> printBoard newBoard >> putStrLn (showGameState newState)


t1 = [
    [W, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, W, Empty, B, Empty, W, Empty, Empty]
  , [Empty, Empty, W, B, W, W, Empty, Empty]
  , [Empty, Empty, Empty, B, W, W, W, Empty]
  , [Empty, Empty, Empty, W, B, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  ]

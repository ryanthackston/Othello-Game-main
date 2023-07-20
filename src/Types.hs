module Types where

-- To play game:
-- cabal repl
-- play W _NEW_BOARD_

data Square = B | W | Empty deriving(Show, Eq)

data GameState = BWon | WWon | Tie | InProgress deriving Show

type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)
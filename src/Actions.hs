module Actions where
import Types
import Lib
    ( _DISPLAY_LOGO_,
      printLogo,
      showGameState,
      switchPlayer,
      printBoard,
      promptPlayer,
      stringToMove,
      dir,
      checkMove,
      filterLists,
      validSquare,
      moveToFL,
      putSquares,
      t1 )
import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)

-- To play game:
-- cabal repl
-- play W _NEW_BOARD_

_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Square
firstPlayer  = _RANDOM_BOOL_ >>= (\i -> if i then return B else return W)

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

showScore :: Board -> String
showScore b = "SCORE   White: " ++ show(fst (countSquares b)) ++ "   " ++ "Black: " ++ show(snd (countSquares b)) 

getGameState :: Board -> GameState
getGameState b
  -- Any empty tile means game in progress
  | elem Empty (concat b) = InProgress
  | uncurry (>) (countSquares t1) = WWon
  | uncurry (<) (countSquares t1) = BWon
  | otherwise = Tie

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState(putSquares p b (moveToFL m (filterLists (checkMove p b m dir))) ), putSquares p b (moveToFL m (filterLists (checkMove p b m dir))))

play :: Player -> Board -> IO ()
play p b = when _DISPLAY_LOGO_ (printLogo >>= putStrLn)  >> printBoard b >> putStrLn (showScore b) >> putStrLn (promptPlayer p) >> getMove p b >>= executeMove
    where
        executeMove :: Move -> IO ()
        executeMove m = let (newState, newBoard) = playMove p b m
            in case newState of
                InProgress -> play (switchPlayer p) newBoard
                _ -> printBoard newBoard >> putStrLn (showGameState newState)

--

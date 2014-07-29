--file: Main.hs
module Main where

import System.Console.ANSI
import System.Random
import System.IO

import Model

main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Miner"
  putStrLn "hello world"
  rand <- newStdGen
  gameLoop standardMine (randoms rand)

data Input = MoveUp
           | MoveDown
           | MoveLeft
           | MoveRight
           | LadderInPlace
           | LadderUp
           | LadderDown
           | LadderLeft
           | LadderRight
           | DoNothing
           | Exit
           deriving (Eq)

-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return MoveUp
    's' -> return MoveDown
    'a' -> return MoveLeft
    'd' -> return MoveRight
    ' ' -> return DoNothing
    'l' -> return LadderInPlace
    'W' -> return LadderUp
    'S' -> return LadderDown
    'A' -> return LadderLeft
    'D' -> return LadderRight
    _   -> getInput

travel :: Mine -> Direction -> [Double] -> Mine
travel old direction rands
  | isInElevator old && movingVertically = rideElevator old direction
  | destinationTile old direction == Dirt = digDirt old direction rands
  | (destinationTile old direction == Air || destinationTile old direction == Elevator || destinationTile old direction == Ladder) &&
      (direction /= Upward || minerTile old == Ladder) = walk old direction
  | otherwise = old -- Cannot move that direction
  where
    movingVertically = (direction == Upward || direction == Downward)

gameLoop :: Mine -> [Double] -> IO ()
gameLoop mine rands = do
  drawMine mine
  input <- if isFalling mine then return MoveDown else getInput
  case input of
    Exit          -> handleExit
    MoveUp        -> gameLoop (travel mine Upward rands) (tail rands)
    MoveDown      -> gameLoop (travel mine Downward rands) (tail rands)
    MoveLeft      -> gameLoop (travel mine Leftward rands) (tail rands)
    MoveRight     -> gameLoop (travel mine Rightward rands) (tail rands)
    LadderInPlace -> gameLoop (makeLadder mine Stationary) (tail rands)
    LadderUp      -> gameLoop (makeLadder mine Upward) (tail rands)
    LadderDown    -> gameLoop (makeLadder mine Downward) (tail rands)
    LadderLeft    -> gameLoop (makeLadder mine Leftward) (tail rands)
    LadderRight   -> gameLoop (makeLadder mine Rightward) (tail rands)
    otherwise     -> gameLoop (mine) (tail rands)

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"

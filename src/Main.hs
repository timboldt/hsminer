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
           | InPlace
           | ForceUp
           | ForceDown
           | ForceLeft
           | ForceRight
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
    ' ' -> return InPlace
    'W' -> return ForceUp
    'S' -> return ForceDown
    'A' -> return ForceLeft
    'D' -> return ForceRight
    _   -> getInput

travel :: Mine -> Direction -> Bool -> [Double] -> Mine
travel old direction force rands
  | isInElevator old && movingVertically = rideElevator old direction
  | (destinationTile old direction == Air) && force = makeLadder old direction
  | (destinationTile old direction == Sandstone || destinationTile old direction == Limestone || destinationTile old direction == Granite) &&
      (direction /= Upward || minerTile old == Ladder) && force = drillRock old direction
  | (destinationTile old direction == Silver || destinationTile old direction == Gold || destinationTile old direction == Platinum || destinationTile old direction == Gems) &&
      (direction /= Upward || minerTile old == Ladder) = collectValuables old direction
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
    Exit       -> handleExit
    MoveUp     -> gameLoop (travel mine Upward     False rands) (tail rands)
    MoveDown   -> gameLoop (travel mine Downward   False rands) (tail rands)
    MoveLeft   -> gameLoop (travel mine Leftward   False rands) (tail rands)
    MoveRight  -> gameLoop (travel mine Rightward  False rands) (tail rands)
    InPlace    -> gameLoop (travel mine Stationary True  rands) (tail rands)
    ForceUp    -> gameLoop (travel mine Upward     True  rands) (tail rands)
    ForceDown  -> gameLoop (travel mine Downward   True  rands) (tail rands)
    ForceLeft  -> gameLoop (travel mine Leftward   True  rands) (tail rands)
    ForceRight -> gameLoop (travel mine Rightward  True  rands) (tail rands)

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"

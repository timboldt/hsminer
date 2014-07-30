module Model where

import System.Console.ANSI
import qualified Data.Map as M

-- x/y coordinate, where (0, 0) is top left corner
type Coord = (Int, Int)

-- Different types of tiles in the mine
data Tile = Dirt      -- Variable tile that is revealed by digging
          | Sandstone -- Soft stone
          | Limestone -- Moderately hard stone
          | Granite   -- Hard stone
          | Bedrock   -- Impenetrable rock used for boundaries
          | Grass     -- Impenetrable surface material for boundaries
          | Water
          | Silver
          | Gold
          | Platinum
          | Gems
          | Ladder
          | Air       -- Empty space
          | Rope      -- Elevator rope (normally an implied entity)
          | Elevator  -- Elevator body (normally an implied entity)
          deriving Eq

data Mine = Mine { mMiner       :: Coord             -- Main character
                 , mElevator    :: Coord             -- Position of the elevator
                 , mTiles       :: M.Map Coord Tile  -- Contents of tiles (or Dirt if not specified)
                 , mMax         :: Coord             -- Coordinates of bottom-rightmost tile
                 , mEnergy      :: Int               -- Energy remaining
                 , mMoneyInBank :: Int               -- Cash in the bank
                 , mCash        :: Int }             -- Cash in hand

standardMineMaxX = 20

standardMineMaxY = 20

standardElevatorXLocation = standardMineMaxX - 2

standardGrassYLocation = 3

standardTiles = M.fromList ( 
                    [((x, y), Bedrock) | x <- [0..standardMineMaxX], y <- [0, standardMineMaxY]]
                 ++ [((x, y), Bedrock) | x <- [0, standardMineMaxX], y <- [1..standardMineMaxY - 1]]
                 ++ [((x, standardGrassYLocation), Grass) | x <- [1..standardMineMaxX - 1], x /= standardElevatorXLocation])

standardMine = Mine { mMiner = (standardElevatorXLocation - 2, standardGrassYLocation - 1)
                    , mElevator = (standardElevatorXLocation, standardGrassYLocation - 1)
                    , mTiles = standardTiles 
                    , mMax = (standardMineMaxX, standardMineMaxY)
                    , mEnergy = 500
                    , mMoneyInBank = 1000
                    , mCash = 0 }

tileAtCoord :: Coord -> Mine -> Tile
tileAtCoord (x, y) mine
  | x == elevatorX = if y == 0              then Bedrock
                     else if y < elevatorY  then Rope
                     else if y == elevatorY then Elevator
                     else if y < maxY       then Air
                     else                        Bedrock
  | otherwise = case M.lookup (x, y) (mTiles mine) of
      Just tile  -> tile
      Nothing -> if y <= standardGrassYLocation then Air else Dirt
  where elevatorX = fst (mElevator mine)
        elevatorY = snd (mElevator mine)
        maxY = snd (mMax mine)

-- Move this to "Controller" or whatever
data Direction = Upward
               | Downward
               | Leftward
               | Rightward
               | Stationary
               deriving Eq

isFalling :: Mine -> Bool
isFalling mine =
  tileAtCoord (x, y) mine == Air && tileAtCoord (x, y + 1) mine == Air
  where (x, y) = mMiner mine

isInElevator :: Mine -> Bool
isInElevator mine = mMiner mine == mElevator mine

minerTile :: Mine -> Tile
minerTile mine = tileAtCoord (mMiner mine) mine

destinationCoord :: Mine -> Direction -> Coord
destinationCoord mine direction = shiftCoord (mMiner mine) direction

destinationTile :: Mine -> Direction -> Tile
destinationTile mine direction = tileAtCoord (destinationCoord mine direction) mine

shiftCoord :: Coord -> Direction -> Coord
shiftCoord (x, y) direction = case direction of
                              Upward     -> (x, y - 1)
                              Downward   -> (x, y + 1)
                              Leftward   -> (x - 1, y)
                              Rightward  -> (x + 1, y)
                              Stationary -> (x, y)

walk :: Mine -> Direction -> Mine
walk old direction = Mine { mMiner       = shiftCoord (mMiner old) direction
                          , mElevator    = mElevator old
                          , mTiles       = mTiles old
                          , mMax         = mMax old
                          , mEnergy      = mEnergy old
                          , mMoneyInBank = mMoneyInBank old
                          , mCash        = mCash old } 

collectValuables :: Mine -> Direction -> Mine
collectValuables old direction
  | value > 0 = Mine { mMiner       = shiftCoord (mMiner old) direction
                     , mElevator    = mElevator old
                     , mTiles       = M.insert (destinationCoord old direction) Air (mTiles old)
                     , mMax         = mMax old
                     , mEnergy      = mEnergy old
                     , mMoneyInBank = mMoneyInBank old
                     , mCash        = (mCash old) + value }
  | otherwise = old
  where
    value = case destinationTile old direction of
      Silver   -> 5
      Gold     -> 10
      Platinum -> 20
      Gems     -> 50
      _        -> 0
 
drillRock :: Mine -> Direction -> Mine
drillRock old direction
  | cost > 0 && (mEnergy old > cost) =
      Mine { mMiner       = shiftCoord (mMiner old) direction
           , mElevator    = mElevator old
           , mTiles       = M.insert (destinationCoord old direction) Air (mTiles old)
           , mMax         = mMax old
           , mEnergy      = (mEnergy old) - cost
           , mMoneyInBank = mMoneyInBank old
           , mCash        = mCash old }
  | otherwise = old
  where
    cost = case destinationTile old direction of
      Sandstone -> 5
      Limestone -> 10
      Granite   -> 20
      _         -> 0

digDirt :: Mine -> Direction -> [Double] -> Mine
digDirt old direction rs
  | (direction == Upward && minerTile old /= Ladder) || newTile /= Air
              = Mine { mMiner       = mMiner old
                     , mElevator    = mElevator old
                     , mTiles       = M.insert (destinationCoord old direction) newTile (mTiles old)
                     , mMax         = mMax old
                     , mEnergy      = mEnergy old
                     , mMoneyInBank = mMoneyInBank old
                     , mCash        = mCash old } 
  | otherwise = Mine { mMiner       = shiftCoord (mMiner old) direction
                     , mElevator    = mElevator old
                     , mTiles       = M.insert (destinationCoord old direction) newTile (mTiles old)
                     , mMax         = mMax old
                     , mEnergy      = mEnergy old
                     , mMoneyInBank = mMoneyInBank old
                     , mCash        = mCash old } 
  where
      df = 4 * (snd (mMiner old)) `div` (snd (mMax old)) + 1
      randTiles = (replicate (10 `div` df) Sandstone) ++ (replicate 5 Limestone) ++ (replicate (2 * df) Granite) ++ (replicate df Bedrock)
               ++ (replicate (4 * df) Silver) ++ (replicate (3 * df) Gold) ++ (replicate (2 * df) Platinum) ++ (replicate df Gems)
               ++ (replicate df Water) ++ (repeat Air)
      r = floor ((head rs) * 100)
      newTile = head (drop r randTiles)

rideElevator :: Mine -> Direction -> Mine
rideElevator old direction
  | newY > 1 && newY < maxY = Mine { mMiner       = shiftCoord (mMiner old) direction
                                   , mElevator    = shiftCoord (mElevator old) direction
                                   , mTiles       = mTiles old
                                   , mMax         = mMax old
                                   , mEnergy      = mEnergy old
                                   , mMoneyInBank = mMoneyInBank old
                                   , mCash        = mCash old } 
  | otherwise = old
  where
    (newX, newY) = shiftCoord (mElevator old) direction
    (maxX, maxY) = mMax old

fallDown :: Mine -> Mine
fallDown old = Mine { mMiner       = shiftCoord (mMiner old) Downward 
                    , mElevator    = mElevator old
                    , mTiles       = mTiles old
                    , mMax         = mMax old
                    , mEnergy      = mEnergy old
                    , mMoneyInBank = mMoneyInBank old
                    , mCash        = mCash old } 

makeLadder :: Mine -> Direction -> Mine
makeLadder old direction
  | destinationTile old direction == Air && fst (destinationCoord old direction) /= fst (mElevator old)
      = Mine { mMiner       = mMiner old
             , mElevator    = mElevator old
             , mTiles       = M.insert (destinationCoord old direction) Ladder (mTiles old)
             , mMax         = mMax old
             , mEnergy      = mEnergy old
             , mMoneyInBank = mMoneyInBank old
             , mCash        = mCash old } 
  | otherwise = old

-- Move this to "View" instead of "Model"
coordToChar :: Coord -> Mine -> Char
coordToChar coord mine 
  | coord == mMiner mine = '*'
  | otherwise = case tileAtCoord coord mine of
        Rope       -> '|'
        Elevator   -> '_'
        Dirt       -> '#'
        Sandstone  -> '1'
        Limestone  -> '2'
        Granite    -> '3'
        Bedrock    -> '='
        Grass      -> '>'
        Water      -> '~'
        Silver     -> 'S'
        Gold       -> 'G'
        Platinum   -> 'P'
        Gems       -> '^'
        Ladder     -> 'H'
        Air        -> ' '

dumpMine :: Mine -> IO () 
dumpMine mine = mapM_ print ([[coordToChar (x, y) mine | x <- [0..(fst (mMax mine))]] | y <- [0..(snd (mMax mine))]])

drawColorChar :: Char -> ColorIntensity -> Color -> ColorIntensity -> Color -> IO ()
drawColorChar char fgIntensity fgColor bgIntensity bgColor = do
  setSGR [ SetColor Foreground fgIntensity fgColor
         , SetColor Background bgIntensity bgColor ]
  putChar char

drawChar :: Char -> IO ()
drawChar char = case char of
  '*' -> drawColorChar '*'  Vivid White   Dull  Black 
  '|' -> drawColorChar '|'  Dull  White   Dull  Black
  '_' -> drawColorChar '_'  Dull  White   Dull  Black 
  '#' -> drawColorChar '#'  Dull  Yellow  Dull  Yellow
  '1' -> drawColorChar '#'  Vivid Yellow  Vivid Yellow
  '2' -> drawColorChar '='  Vivid Black   Dull  White
  '3' -> drawColorChar '#'  Dull  Red     Vivid Black
  '=' -> drawColorChar '#'  Vivid Black   Vivid Black
  '>' -> drawColorChar '#'  Vivid Green   Dull  Green
  '~' -> drawColorChar '~'  Vivid Blue    Dull  Blue
  'S' -> drawColorChar '+'  Dull  White   Dull  Black
  'G' -> drawColorChar '+'  Vivid Yellow  Dull  Black
  'P' -> drawColorChar '+'  Vivid White   Dull  Black
  '^' -> drawColorChar '+'  Vivid Red     Dull  Black
  'H' -> drawColorChar 'H'  Dull  White   Dull  Black
  ' ' -> drawColorChar ' '  Dull  Black   Dull  Black
  _   -> drawColorChar char Dull  White   Dull  Black

drawMine :: Mine -> IO ()
drawMine mine = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  setSGR [ Reset ]
  where
    (x', y') = mMax mine
    chars    = ([[coordToChar (x, y) mine | x <- [0..x']] | y <- [0..y']])

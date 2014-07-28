module Model where

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

data Mine = Mine { mMiner    :: Miner             -- Main character
                 , mElevator :: Coord             -- Position of the elevator
                 , mTiles    :: M.Map Coord Tile  -- Contents of tiles (or Dirt if not specified)
                 , mMax      :: Coord }           -- Coordinates of bottom-rightmost tile

data Miner = Miner { mLocation :: Coord
                   , mEnergy :: Int
                   , mMoneyInBank :: Int
                   , mCash :: Int }

standardMineMaxX = 50

standardMineMaxY = 20

standardElevatorXLocation = standardMineMaxX - 2

standardGrassYLocation = 3

standardMiner = Miner { mLocation = (standardElevatorXLocation - 3, standardGrassYLocation - 1)
                      , mEnergy = 500
                      , mMoneyInBank = 1000
                      , mCash = 0 }

standardTiles = M.fromList ( 
                    [((x, y), Bedrock) | x <- [0..standardMineMaxX], y <- [0, standardMineMaxY]]
                 ++ [((x, y), Bedrock) | x <- [0, standardMineMaxX], y <- [1..standardMineMaxY - 1]]
                 ++ [((x, standardGrassYLocation), Grass) | x <- [1..standardMineMaxX - 1], x /= standardElevatorXLocation])

standardMine = Mine { mMiner = standardMiner
                    , mElevator = (standardElevatorXLocation, standardGrassYLocation - 1)
                    , mTiles = standardTiles 
                    , mMax = (standardMineMaxX, standardMineMaxY) }

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

-- Move this to "View" instead of "Model"
coordToChar :: Coord -> Mine -> Char
coordToChar coord mine 
  | coord == mLocation (mMiner mine) = '*'
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

p = [[coordToChar (x, y) standardMine | x <- [0..standardMineMaxX]] | y <- [0..standardMineMaxY]]

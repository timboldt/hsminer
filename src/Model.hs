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

-- Move this to "View" instead of "Model"
coordToChar :: Coord -> Mine -> Char
coordToChar coord mine 
  | coord == mLocation (mMiner mine) = '*'
  | coord == elevatorCoord           = '_'
  | fst coord == fst elevatorCoord && snd coord < snd elevatorCoord && snd coord > 0 = '|'
  | fst coord == fst elevatorCoord && snd coord > snd elevatorCoord && snd coord < snd (mMax mine) = ' '
  | otherwise = case M.lookup coord (mTiles mine) of
      Just Dirt       -> '#'
      Just Sandstone  -> '1'
      Just Limestone  -> '2'
      Just Granite    -> '3'
      Just Bedrock    -> '='
      Just Grass      -> '>'
      Just Water      -> '~'
      Just Silver     -> 'S'
      Just Gold       -> 'G'
      Just Platinum   -> 'P'
      Just Gems       -> '^'
      Just Ladder     -> 'H'
      Just Air        -> ' '
      Nothing         -> if snd coord <= standardGrassYLocation then ' ' else '#'
  where elevatorCoord = mElevator mine

p = [[coordToChar (x, y) standardMine | x <- [0..standardMineMaxX]] | y <- [0..standardMineMaxY]]

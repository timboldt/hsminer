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
          deriving Eq

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

standardMiner = Miner { mLocation = (standardElevatorXLocation - 2, standardGrassYLocation - 1)
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
  where (x, y) = mLocation (mMiner mine)

isInElevator :: Mine -> Bool
isInElevator mine = mLocation (mMiner mine) == mElevator mine

minerTile :: Mine -> Tile
minerTile mine = tileAtCoord (mLocation (mMiner mine)) mine

destinationCoord :: Mine -> Direction -> Coord
destinationCoord mine direction = shiftCoord (mLocation (mMiner mine)) direction

destinationTile :: Mine -> Direction -> Tile
destinationTile mine direction = tileAtCoord (destinationCoord mine direction) mine

shiftCoord :: Coord -> Direction -> Coord
shiftCoord (x, y) direction = case direction of
                              Upward     -> (x, y - 1)
                              Downward   -> (x, y + 1)
                              Leftward   -> (x - 1, y)
                              Rightward  -> (x + 1, y)
                              Stationary -> (x, y)

moveMiner :: Miner -> Direction -> Miner
moveMiner old direction = Miner { mLocation    = shiftCoord (mLocation old) direction 
                                , mEnergy      = mEnergy old
                                , mMoneyInBank = mMoneyInBank old
                                , mCash        = mCash old } 

walk :: Mine -> Direction -> Mine
walk old direction = Mine { mMiner    = moveMiner (mMiner old) direction
                          , mElevator = mElevator old
                          , mTiles    = mTiles old
                          , mMax      = mMax old }

digDirt :: Mine -> Direction -> Mine
digDirt old direction 
  | (direction == Upward && minerTile old /= Ladder)
              = Mine { mMiner    = mMiner old
                     , mElevator = mElevator old
                     , mTiles    = M.insert (destinationCoord old direction) Air (mTiles old)
                     , mMax      = mMax old }
  | otherwise = Mine { mMiner    = moveMiner (mMiner old) direction
                     , mElevator = mElevator old
                     , mTiles    = M.insert (destinationCoord old direction) Air (mTiles old)
                     , mMax      = mMax old }

rideElevator :: Mine -> Direction -> Mine
rideElevator old direction = Mine { mMiner    = moveMiner (mMiner old) direction
                                  , mElevator = shiftCoord (mElevator old) direction
                                  , mTiles    = mTiles old
                                  , mMax      = mMax old }

fallDown :: Mine -> Mine
fallDown old = Mine { mMiner    = moveMiner (mMiner old) Downward 
                    , mElevator = mElevator old
                    , mTiles    = mTiles old
                    , mMax      = mMax old }

makeLadder :: Mine -> Direction -> Mine
makeLadder old direction
  | destinationTile old direction == Air && fst (destinationCoord old direction) /= fst (mElevator old)
      = Mine { mMiner    = mMiner old
             , mElevator = mElevator old
             , mTiles    = M.insert (destinationCoord old direction) Ladder (mTiles old)
             , mMax      = mMax old }
  | otherwise = old

travel :: Mine -> Direction -> Mine
travel old direction
  | isFalling old = fallDown old
  | isInElevator old && movingVertically = rideElevator old direction
  | destinationTile old direction == Dirt = digDirt old direction
  | (destinationTile old direction == Air || destinationTile old direction == Elevator) &&
      (direction /= Upward || minerTile old == Ladder) = walk old direction
  | otherwise = old -- Cannot move that direction
  where movingVertically = (direction == Upward || direction == Downward)

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

dumpMine :: Mine -> IO () 
dumpMine mine = mapM_ print ([[coordToChar (x, y) mine | x <- [0..(fst (mMax mine))]] | y <- [0..(snd (mMax mine))]])

p0 = standardMine

p1 = travel (travel p0 Rightward) Rightward

p2 = travel (travel (travel (travel p1 Downward) Downward) Downward) Downward

p3 = travel (travel (travel p2 Leftward) Leftward) Leftward

p4 = travel (travel (travel (makeLadder p3 Stationary)  Upward) Leftward) Leftward

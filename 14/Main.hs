module Main where

import Data.IntMap (IntMap); import qualified Data.IntMap as IM
import Data.IntSet (IntSet); import qualified Data.IntSet as IS
import Data.Ix
import Data.List


-- | Sand

type P = (Int,Int)
data Status = Stable | CanDrop P | Disappeared | Covered deriving Show

getStatus :: Int -> Int -> Grid -> P -> Status
getStatus voidLvl floorLvl grid (i,j)
  | cellBlocked grid (i,j)  = Covered
  | j == voidLvl            = Disappeared
  | j+1 == floorLvl         = Stable
  | cellFree grid (i,j+1)   = CanDrop (i,j+1)
  | cellFree grid (i-1,j+1) = CanDrop (i-1,j+1)
  | cellFree grid (i+1,j+1) = CanDrop (i+1,j+1)
  | otherwise               = Stable

simulate1 :: Int -> Int -> Grid -> Maybe Grid
simulate1 voidLvl floorLvl grid = go (500,0) where
  go p = case getStatus voidLvl floorLvl grid p of
    CanDrop p' -> go p'
    Stable     -> Just (putCell grid p)
    _          -> Nothing

simulate :: Int -> Int -> Grid -> [Grid]
simulate voidLvl floorLvl cave = go cave where
  go grid = case simulate1 voidLvl floorLvl grid of
    Nothing     -> []
    Just grid' -> grid' : go grid'


-- | The Grid

type Line = (P,P)
type Grid = IntMap IntSet

putCell :: Grid -> P -> Grid
putCell rows (i,j) = IM.alter f j rows where
  f Nothing     = Just (IS.singleton i)
  f (Just cols) = Just (IS.insert i cols)

cellBlocked :: Grid -> P -> Bool
cellBlocked rows (i,j) = case IM.lookup j rows of
  Just cols -> IS.member i cols
  Nothing   -> False

cellFree :: Grid -> P -> Bool
cellFree grid = not . cellBlocked grid

expandLine :: Line -> [P]
expandLine (p1,p2) = case range (p1,p2) of
  [] -> range (p2,p1)
  xs -> xs

drawLine :: Grid -> Line -> Grid
drawLine grid l = foldl' putCell grid (expandLine l)

drawWalls :: Grid -> [Line] -> Grid
drawWalls grid ls = foldl' drawLine grid ls

buildGrid :: [[Line]] -> Grid
buildGrid = foldl' drawWalls IM.empty


-- | Loader

data K = N Int | Arrow deriving (Eq,Ord,Read,Show)
tokenize :: String -> [K]
tokenize (' ':'-':'>':' ':more)  = Arrow : tokenize more
tokenize (',':more)              = N i : tokenize rest where [(i,rest)] = reads more
tokenize []                      = []
tokenize more                    = N i : tokenize rest where [(i,rest)] = reads more

parseLine :: [K] -> [Line]
parseLine []                                     = []
parseLine (N a : N b : Arrow : N c : N d : more) = ((a,b),(c,d)) : case more of
  [] -> []
  _  -> parseLine (N c : N d : more)

loadData :: FilePath -> IO [[Line]]
loadData = fmap (map (parseLine . tokenize) . lines) . readFile

-- | Main

main = do
  wallsList <- loadData "input"
  let cave   = buildGrid wallsList
  let maxRow = maximum (IM.keys cave)
  print (length (simulate (maxRow + 3) (maxRow + 4) cave))
  print (length (simulate (maxRow + 3) (maxRow + 2) cave))

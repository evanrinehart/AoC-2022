module Main where

import qualified Data.Map as M; import Data.Map (Map)

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq,Ord,Show)
type Cost = (Int,Resource)
data Blueprint = BP 
  { bpID       :: Int
  , bpCosts    :: Map Resource [Cost] }
      deriving Show

main = do
  bps <- loadData "input"
  mapM_ print bps
  mapM_ (print . maxGeodes 24) bps


data World = World
  { wStocks  :: Map Resource Int
  , wBots    :: Map Resource Int }
      deriving Show

blankWorld = World { wStocks = stocks, wBots = bots } where
  stocks = M.fromList [(Ore,0),(Clay,0),(Obsidian,0),(Geode,0)]
  bots   = M.fromList [(Ore,1),(Clay,0),(Obsidian,0),(Geode,0)]

qualityLevel :: Int -> Blueprint -> Int
qualityLevel t bp = bpID bp * maxGeodes t bp

-- how many geodes can be cracked in the given time limit
maxGeodes :: Int -> Blueprint -> Int
maxGeodes t bp = 0

-- which kind of bot can be built now
--possibleActions :: World -> Blueprint -> [Resource]
possibleActions (World stocks _) (BP _ costs) = []



-- loader

gut c = drop 1 . dropWhile (/= c)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn z l@(x:xs)
  | x==z      = splitOn z xs
  | otherwise = let (h,t) = break (==z) l in h:(splitOn z t)

parseBlueprint str = BP i costs where
  [header,body] = splitOn ':' str
  i = read (gut ' ' header) :: Int
  recips = map (drop 4 . words) (splitOn '.' body)
  [r1,r2,r3,r4] = map parseCost recips
  costs = M.fromList [(Ore,r1),(Clay,r2),(Obsidian,r3),(Geode,r4)]

parseCost :: [String] -> [Cost]
parseCost []                 = []
parseCost (s1:s2:"and":more) = parseIngredient (read s1) s2 : parseCost more
parseCost (s1:s2:[])         = [parseIngredient (read s1) s2]

parseIngredient n "ore"      = (n, Ore)
parseIngredient n "clay"     = (n, Clay)
parseIngredient n "obsidian" = (n, Obsidian)

loadData = fmap (map parseBlueprint . lines) . readFile


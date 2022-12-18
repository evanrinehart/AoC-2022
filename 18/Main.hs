module Main where

import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map, (!))

import Control.Exception

data V3 = V3 !Int !Int !Int
  deriving (Eq,Ord,Read,Show)

type Lava = Set V3

parseLine :: String -> V3
parseLine str =
  let [(a,',':rest)]  = reads str   in
  let [(b,',':rest')] = reads rest  in
  let [(c,_)]         = reads rest' in
  V3 a b c

loadData :: FilePath -> IO [V3]
loadData = fmap (map parseLine . lines) . readFile

surroundings :: V3 -> [V3]
surroundings (V3 a b c) =
  [V3 (a+1) b c
  ,V3 (a-1) b c
  ,V3 a (b+1) c
  ,V3 a (b-1) c
  ,V3 a b (c+1)
  ,V3 a b (c-1)]

countNeighbors :: V3 -> Lava -> Int
countNeighbors p lava = sum (map f (surroundings p)) where
  f q = if S.member q lava then 1 else 0

makeNMap :: [V3] -> Lava -> Map V3 Int
makeNMap ps lava = M.fromList (map f ps) where
  f p = (p, countNeighbors p lava)

countNeighbors2 :: V3 -> Lava -> Int
countNeighbors2 p lava = sum (map f (surroundings p)) where
  f q
    | S.member q lava             = 1
    | floodAny outOfBounds lava q = 0
    | otherwise                   = 1

makeNMap2 :: [V3] -> Lava -> Map V3 Int
makeNMap2 ps lava = M.fromList (map f ps) where
  f p = (p, countNeighbors2 p lava)


surface :: [V3] -> Map V3 Int -> Int -> Int
surface [] _ accum = accum
surface (p:ps) nmap accum =
  let accum' = accum + 6 - nmap ! p
  in accum' `seq` surface ps nmap accum'

bounds :: [V3] -> [Int]
bounds ps = [x0,x1,y0,y1,z0,z1] where
  f (V3 x y z) = x
  g (V3 x y z) = y
  h (V3 x y z) = z
  x0 = minimum xs
  x1 = maximum xs
  y0 = minimum ys
  y1 = maximum ys
  z0 = minimum zs
  z1 = maximum zs
  xs = map f ps
  ys = map g ps
  zs = map h ps

outOfBounds (V3 x y z) = 
  x < 0 || y < 0 || z < 0 ||
  x > 20 || y > 20 || z > 20

main = do
  triples <- loadData "input"
  --print (bounds triples)
  let lava = S.fromList triples
  let nmap = makeNMap2 triples lava

  --evaluate nmap

  print (surface triples nmap 0)
  pure ()


air :: V3 -> Lava -> [V3]
air p lava = filter (not . flip S.member lava) (surroundings p)

unvisited :: V3 -> Set V3 -> Lava -> [V3]
unvisited p seen lava = filter (not . flip S.member seen) (air p lava)

floodAny :: (V3 -> Bool) -> Lava -> V3 -> Bool
floodAny f lava start = any f (flood lava start)

flood :: Lava -> V3 -> [V3]
flood lava start = go start S.empty [] where
  go p seen ps =
    let ps'   = unvisited p seen lava ++ ps in
    let seen' = S.insert p seen in
    case ps' of
      []     -> [p]
      (x:xs) -> p : go x seen' xs

module Main where

import Data.Char
import Data.Array
import Data.List
import Data.Ord

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import AStar

type Point = (Int,Int)
type Grid = Array (Int,Int) Int

main = do
  (grid, start, end) <- loadData "input"

  let astar = findPathFromTo (makeSpace end grid)
  print (length (astar start end) - 1)

  let starts = map (\j -> (1,j)) [1..41]
  print (minimum (map (length . flip astar end) starts) - 1)

makeSpace :: Point -> Grid -> PathSpace (Int,Int)
makeSpace (ex,ey) grid = PathSpace{linkWeight=lw,heuristic=h,neighborhoodOf=nh} where
  (width,height) = snd (bounds grid)
  lw from to     = 719
  h (x,y)        = (ex - x) + (ey - y)
  nh             = neighboring width height grid

neighboring :: Int -> Int -> Grid -> Point -> [Point]
neighboring w h grid (x,y) = p1 ++ p2 ++ p3 ++ p4 where
  cutoff = grid ! (x,y) + 1
  p1 = if x > 1 && grid ! p <= cutoff then [p] else [] where p = (x-1,y)
  p2 = if x < w && grid ! p <= cutoff then [p] else [] where p = (x+1,y) 
  p3 = if y > 1 && grid ! p <= cutoff then [p] else [] where p = (x,y-1)
  p4 = if y < h && grid ! p <= cutoff then [p] else [] where p = (x,y+1)





-- notes, 181 x 41
loadData :: FilePath -> IO (Grid, Point, Point)
loadData path = do
  rawLines <- fmap lines (readFile path)
  let numRows = length rawLines
  let numCols = length (head rawLines)
  let assocs = terrainAssocs rawLines
  let grid = makeGrid numCols numRows assocs
  let start = findPoint 'S' assocs
  let end   = findPoint 'E' assocs
  return (grid, start, end)

charToElevation 'S' = charToElevation 'a'
charToElevation 'E' = charToElevation 'z'
charToElevation c   = ord c - ord 'a' 

terrainAssocs :: [String] -> [(Point, Char)]
terrainAssocs = concat . zipWith (\j str -> zipWith (\i c -> ((i,j), c)) [1..] str) [1..]

makeGrid :: Int -> Int -> [(Point,Char)] -> Grid
makeGrid w h = fmap charToElevation . array ((1,1), (w,h))

findPoint d hay = case find (\(_, c) -> c==d) hay of
  Just (p,_) -> p
  Nothing    -> error "findPoint: point not found"


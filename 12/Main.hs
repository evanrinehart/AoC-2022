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
  (grid, start, end, altStarts) <- loadData "input"

  let cameFrom = M.empty
  let gScore = M.singleton start 0

  let astar = makeAStar grid

  let p = astar start end
  print (length p)

  let try s = length (astar s end)
  mapM_ (\p -> print (p, try p)) (map (\j -> (1,j)) [1..41])
--  print (minimum (map try ss))

makeAStar :: Grid -> Point -> Point -> [Point]
makeAStar grid = findPathFromTo (makeSpace grid)

makeSpace :: Grid -> PathSpace (Int,Int)
makeSpace grid = PathSpace{linkWeight=lw,heuristic=h,neighborhoodOf=nh,encodePoint=en} where
  width = let (_,(w,_)) = bounds grid in w
  lw from to =
    let h1 = grid ! from
        h2 = grid ! to
    in if h2 - h1 > 1 then 99999 else 141
  h (x,y) = (159 - x) + (21 - y)
  nh (x,y) = filter (not . outOfBounds) ps where
    ps = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    outOfBounds (a,b) = a < 1 || b < 1 || a > 181 || b > 41
  en (x,y) = y*width + x





-- notes, 181 x 41
loadData :: FilePath -> IO (Grid, Point, Point, [Point])
loadData path = do
  rawLines <- fmap lines (readFile path)
  let numRows = length rawLines
  let numCols = length (head rawLines)
  let assocs = terrainAssocs rawLines
  let grid = makeGrid numCols numRows assocs
  let start = findPoint 'S' assocs
  let end   = findPoint 'E' assocs
  return (grid, start, end, possibleStarts assocs)

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

possibleStarts :: [(Point,Char)] -> [Point]
possibleStarts = map fst . filter (\(p,c) -> c == 'a')


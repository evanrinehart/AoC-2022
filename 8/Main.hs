module Main where

import Data.Char
import Data.Array
import Data.List

type Point = (Int,Int)
type Ray   = [Point]
type Grid  = Array (Int,Int) Int

main = do
  grid <- loadData "input"
  print (perimeter grid + countIf (visible grid) (interior grid))
  print (maximum (map (viewScore grid) (indices grid)))

visible :: Grid -> Point -> Bool
visible grid p = any (visibleAlong grid p) (rays grid p)

visibleAlong :: Grid -> Point -> Ray -> Bool
visibleAlong grid (x,y) ixs = all (< h) hs where
  h  = grid ! (x,y)
  hs = map (grid !) ixs

viewScore :: Grid -> Point -> Int
viewScore grid p = product (viewDists grid p)

viewDists :: Grid -> Point -> [Int]
viewDists grid p = map (viewDistAlong grid p) (rays grid p)

-- bad weird
viewDistAlong :: Grid -> Point -> Ray -> Int
viewDistAlong grid p ixs = go 0 ixs where
  myH = grid ! p
  go n (i:is) = let h = grid ! i in if h >= myH then n+1 else go (n+1) is
  go n []     = n

rays :: Grid -> Point -> [Ray]
rays grid p = [north grid p, west grid p, south grid p, east grid p]

north :: Grid -> Point -> Ray
north grid (x,y) = map (\j -> (x,j)) [y-1,y-2..1]
west  grid (x,y) = map (\i -> (i,y)) [x-1,x-2..1]
south grid (x,y) = let (_,d) = dims grid in map (\j -> (x,j)) [y+1,y+2..d]
east  grid (x,y) = let (w,_) = dims grid in map (\i -> (i,y)) [x+1,x+2..w]

-- boring

loadData :: FilePath -> IO Grid
loadData path = do
  rows <- fmap lines (readFile path) :: IO [String]
  let height = length rows
  let width  = length (head rows)
  return $ listArray ((1,1), (width,height)) (map digitToInt (concat (transpose rows)))

dims grid      = let ((_,_),(w,d)) = bounds grid in (w,d)
interior grid  = let (w,d) = dims grid in range ((2,2),(w-1,d-1))
perimeter grid = let (w,d) = dims grid in w + w + d + d - 4

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

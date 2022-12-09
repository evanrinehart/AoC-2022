module Main where

import Data.List
import Data.Char

type Point = (Int,Int)
type Diff  = (Int,Int)

diff :: Point -> Point -> Diff
diff (a,b) (c,d) = (a-c, b-d)

shift :: Diff -> Point -> Point
shift (dx,dy) (x,y) = (x+dx, y+dy)

touching :: Diff -> Bool
touching (dx,dy) = abs dx <= 1 && abs dy <= 1

clip :: Diff -> Diff
clip (x,y) = (signum x, signum y)

catchUp :: Point -> Point -> Point
catchUp h t =
  let d = diff h t in
  if touching d
    then t
    else shift (clip (diff h t)) t

charToDir :: Char -> Diff
charToDir 'U' = ( 0, 1)
charToDir 'D' = ( 0,-1)
charToDir 'L' = (-1, 0)
charToDir 'R' = ( 1, 0)

parseMove :: String -> [Diff]
parseMove (c1:' ':rest) = case reads rest of
  [(n,_)] -> replicate n (charToDir c1)

loadData :: FilePath -> IO [Diff]
loadData path = fmap (concatMap parseMove . lines) (readFile path)

step :: Diff -> (Point,Point) -> (Point,Point)
step d (t,h) =
  let h' = shift d h in
  (catchUp h' t, h')

stepN :: Diff -> [Point] -> [Point]
stepN mainD (p:oints) = scanl catchUp (shift mainD p) oints

simulate :: Point -> [Diff] -> [(Point,Point)]
simulate start ds = scanl (flip step) (start,start) ds

simulateN :: Int -> Point -> [Diff] -> [[Point]]
simulateN n start ds = scanl (flip stepN) (replicate n (0,0)) ds

main = do
  ds <- loadData "input"
  let trail2 = map last (simulateN 2 (0,0) ds)
  print (length (nub trail2))
  let trail10 = map last (simulateN 10 (0,0) ds)
  print (length (nub trail10))

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

stepN :: Diff -> [Point] -> [Point]
stepN d (p:oints) = scanl catchUp (shift d p) oints

simulateN :: Int -> [Diff] -> [[Point]]
simulateN n ds = scanl (flip stepN) (replicate n (0,0)) ds

main = do
  ds <- loadData "input"
  let trail2 = map last (simulateN 2 ds)
  print (length (nub trail2))
  let trail10 = map last (simulateN 10 ds)
  print (length (nub trail10))

-- boring

loadData :: FilePath -> IO [Diff]
loadData path = fmap (concatMap parseMove . lines) (readFile path)

parseMove :: String -> [Diff]
parseMove (c1:' ':rest) = case reads rest of
  [(n,_)] -> replicate n (charToDir c1)

charToDir :: Char -> Diff
charToDir 'U' = ( 0, 1)
charToDir 'D' = ( 0,-1)
charToDir 'L' = (-1, 0)
charToDir 'R' = ( 1, 0)

